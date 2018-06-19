#' Store an access token
#'
#' Stores an Asana API access token for the R session.
#' @param access_token A string giving an access token to authenticate the API
#' call.
#' @return Invoked for the side effect of setting the environment variable
#' \code{ASANA_ACCESS_TOKEN} to the input.
#' @details See the developer documentation for how to get an access token.
#' \url{https://asana.com/developers/documentation/getting-started/auth}
#' @export
store_access_token <- function(access_token) {
  assert_is_access_token(access_token)
  Sys.setenv(ASANA_ACCESS_TOKEN = access_token)
}

#' Throw an error if the access token is malformed.
#' @importFrom assertive.types is_a_string
assert_is_access_token <- function(access_token) {
  if(is.na(access_token)) {
    stop("No Asana API access token was found.")
  }
  if(!is_a_string(access_token)) {
    stop("The Asana API access token is not a string.")
  }
  if(nchar(access_token) != 34L) {
    stop("The Asana API access token does not contain 34 characters.")
  }
  invisible(access_token)
}
