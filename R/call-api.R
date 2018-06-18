#' Call the Asana api
#'
#' @param endpoint A string describing the URL to access.
#' @param access_token A string giving an access token to authenticate the API
#' call. See \code{\link{get_access_token}}.
#' @return A list. The exact contents depend upon the endpoint.
#' @importFrom httr add_headers
#' @importFrom httr GET
#' @importFrom httr stop_for_status
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @noRd
call_asana_api <- function(endpoint, access_token, ...) {
  query <- list(...)
  response <- GET(
    paste0("https://app.asana.com/api/1.0/", endpoint),
    config = add_headers(Authorization = paste("Bearer", access_token)),
    query = query
  )
  stop_for_status(response)
  txt <- content(response, as = "text")
  fromJSON(txt)
}
