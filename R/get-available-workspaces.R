#' List Available Workspaces in the Asana
#'
#' @param access_token An Asana access token.
#' @return A list containing a dataframe of all available workspaces to the Asana user.
#' @references \url{https://asana.com/developers/api-reference/workspaces#get}
#' @examples
#' \donttest{
#' ## Marked as don't test because an access token is needed
#' get_all_workspaces()
#' }
#' @export

get_all_workspaces <- function(access_token = Sys.getenv("ASANA_ACCESS_TOKEN")){

  results <- call_asana_api("workspaces", access_token = access_token)
  fix_ids(results$data)

}
