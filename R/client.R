#' Dispatch a GET request to Asana
#'
#' @param endpoint endpoint
#' @param ... query parameters
#' @param .token access token
#' @import httr
#' @export
asn_get <- function(endpoint, ..., options = list(),
    .token = Sys.getenv("ASANA_ACCESS_TOKEN")) {
  response <- GET(
    url = paste0("https://app.asana.com/api/1.0", endpoint),
    config = add_headers(Authorization = paste("Bearer", .token)),
    query = append(options, list(...))
  )
  stop_for_status(response)
  .process_response(response, endpoint)
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

  response <- httr::PUT(
    url = paste0("https://app.asana.com/api/1.0", endpoint),
    config = httr::add_headers(Authorization = paste("Bearer", .token)),
    body = append(list(...), options)
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

# Process response from Asana API
.process_response <- function(response, endpoint){
  if (http_type(response) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(
    content(response, as = "text"),
    flatten = getOption('asana.response.flatten', TRUE)
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
  possibly(as_data_frame, identity)(out)
}

#' @export
#' @importFrom dplyr as_data_frame
as_data_frame.asana_api <- function(x, ...){
  d <- x$content$data
  d1 <- d %>%
    as_data_frame()
  class(d1) <- c('tbl_asana', class(d1))
  return(d1)
}

#' @export
print.tbl_asana <- function(x, ...){
  print(fix_all_ids(x))
}

asn_process_response <- function(data){
  results <- jsonlite::fromJSON(txt)
  if ('data' %in% names(results)){
    results$data$id = asana:::fix_ids(results$data$id)
    return(results$data)
  } else {
    return(results)
  }
}

fix_all_ids <- function(d){
  d %>%
    mutate_at(vars(contains("id")), ~ {
      if (is.numeric(.x) && nchar(.x) >= 10){
        fix_ids(.x)
      } else {
        .x
      }
    })
}
