#' Dispatch a GET request to Asana
#' We first attempt the GET request as is
#' If that fails, a series of paginated requests with a size of 100 are initiated.
#'
#' @param endpoint endpoint
#' @param ... query parameters
#' @param .token access token
#' @import httr
#' @importFrom purrr possibly
#' @export
asn_get <- function(endpoint, ..., options = list(),
    .token = Sys.getenv("ASANA_ACCESS_TOKEN")) {

  tryCatch({
    out <- .do_get_call(endpoint, token = .token, ..., options = options)
    res <- purrr::possibly(as_tibble, out)(out)

    return(res)
  },
  error = function(e){
    message("Falling back to pagination")
    out <- .do_get_call(endpoint, token = .token, ..., options = append(options, list(limit = "100")))
    res <- purrr::possibly(as_tibble, out)(out)
    while (!is.null(out$content$next_page$path)) {
      next_endpoint <- out$content$next_page$path
      out <- .do_get_call(next_endpoint, token = .token, ..., options = append(options, list(limit = "100")))
      res <- bind_rows(res, purrr::possibly(as_tibble, out)(out))
    }

    return(res)
  })
}


#' Dispatch a POST request to Asana
#'
#' @inheritParams asn_get
#' @export
#' @examples
#' \dontrun{
#'   asn_post('/teams/730118080732127/projects', name = 'My New Project')
#' }
asn_post <- function(endpoint, ..., options = list(),
    .token = Sys.getenv("ASANA_ACCESS_TOKEN")){
  check_for_token(.token)
  response <- httr::POST(
    url = paste0("https://app.asana.com/api/1.0", endpoint),
    config = add_headers(Authorization = paste("Bearer", .token)),
    body = append(list(...), options)
  )
  stop_for_status(response)
  .process_response(response, endpoint)
}

#' Dispatch a PUT request to Asana
#'
#' @inheritParams asn_get
#' @export
#' @examples
#' \dontrun{
#'   asn_put('/projects/760385849063788', name = 'New Name')
#' }
asn_put <- function(endpoint, ..., options = list(),
    .token = Sys.getenv("ASANA_ACCESS_TOKEN")){
  check_for_token(.token)
  body = append(list(data = list(...)), options)
  response <- httr::PUT(
    url = paste0("https://app.asana.com/api/1.0", endpoint),
    config = httr::add_headers(Authorization = paste("Bearer", .token)),
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    httr::content_type_json()
  )
  stop_for_status(response)
  .process_response(response, endpoint)
}

#' Print response from Asana API
#'
#' @export
print.asana_api <- function(x, ...) {
  scipen <- getOption('scipen'); on.exit(options(scipen = scipen))
  options(scipen = 22)
  cat("<Asana ", x$path, ">\n", sep = "")
  str(x$content)
  invisible(x)
}

# Do GET call to Asana API
.do_get_call <- function(endpoint, token, ..., options = list()){
  response <- GET(
    url = paste0("https://app.asana.com/api/1.0", endpoint),
    config = add_headers(Authorization = paste("Bearer", token)),
    query = append(options, list(...))
  )
  stop_for_status(response)
  out <- .process_response(response, endpoint)

  return(out)
}


# Process response from Asana API
.process_response <- function(response, endpoint){
  if (http_type(response) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  # TODO: Use jsonlite::flatten in post-processing
  parsed <- jsonlite::fromJSON(
    content(response, as = "text"),
    # flatten = getOption('asana.response.flatten', TRUE)
  )

  if (http_error(response)) {
    stop(
      sprintf(
        "Asana API request failed [%s]\n%s\n",
        status_code(response),
        jsonlite::toJSON(parsed$errors, auto_unbox = TRUE, pretty = TRUE),
      ),
      call. = FALSE
    )
  }

  out <- structure(
    list(
      content = parsed,
      path = endpoint,
      response = response
    ),
    class = "asana_api"
  )
  out
}

#' @export
#' @importFrom dplyr as_tibble
#' @importFrom jsonlite flatten
#' @importFrom purrr map
as_tibble.asana_api <- function(x, ...){
  d <- x$content$data
  d %>%
    fix_all_ids() %>%
    jsonlite::flatten() %>%
    dplyr::as_tibble()
}

asn_process_response <- function(data){
  results <- jsonlite::fromJSON(txt)
  if ('data' %in% names(results)){
    results$data$id = fix_ids(results$data$id)
    return(results$data)
  } else {
    return(results)
  }
}

fix_all_ids <- function(d1){
  for (nm in names(d1)){
    if (is.data.frame(d1[[nm]])){
      d1[[nm]]$id <- fix_ids(d1[[nm]])
    } else if (is.list(d1[[nm]])){
      d1[[nm]] <- d1[[nm]] %>% purrr::map(~ possibly(fix_ids_in_list, .x)(.x))
    }
  }
  d1$id <- fix_ids(d1$id)
  return(d1)
}

fix_ids_in_list <- function(.x){
  if ('id' %in% names(.x)){
    .x$id <- fix_ids(.x$id)
  }
  .x
}

check_for_token <- function(.token){
  if (.token == ""){
    stop("You need an API access token from Asana. You can find instructions on how to get one here: https://github.com/datacamp/asana/blob/master/README.md", call. = FALSE)
  }
}
