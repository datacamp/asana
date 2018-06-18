#' List Available Workspaces in the Asana
#'
#' @return A list containing a dataframe of all available workspaces to the Asana user.
#' @export

list_asana_workspaces <- function(access_token){

  call_asana_api("workspaces", access_token = access_token)

}
