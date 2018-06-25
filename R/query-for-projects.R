#' List all available projects in Asana
#'
#' @param archived TRUE, FALSE, or NA for archived, live, or all projects, respectively. Defaults to NA.
#' @param access_token An Asana access token.
#' @return A list containing a dataframe of all available projects to the Asana user.
#' @references \url{https://asana.com/developers/api-reference/projects#query}
#' @examples
#' \donttest{
#' ## Marked as don't test because an access token is needed
#' get_all_projects()
#' }
#' @export
get_all_projects <- function(archived = NA, access_token = Sys.getenv("ASANA_ACCESS_TOKEN")){

  archived <- fix_archived(archived)
  results <- call_asana_api("projects", access_token = access_token, archived = archived)
  fix_ids(results$data)

}

#' List a specific workspace's projects in Asana
#'
#' @param workspace_id An Asana workspace ID
#' @param archived TRUE, FALSE, or NA for archived, live, or all projects, respectively. Defaults to NA.
#' @param access_token An Asana access token.
#' @return A list containing a dataframe of all projects available in a specific workspace
#' @references \url{https://asana.com/developers/api-reference/projects#query}
#' @examples
#' \donttest{
#' ## Marked as don't test because an access token is needed
#' get_workspace_projects()
#' }
#' @export
get_workspace_projects <- function(workspace_id, archived = NA, access_token = Sys.getenv("ASANA_ACCESS_TOKEN")){

  archived <- fix_archived(archived)
  endpoint <- paste0("workspaces/", workspace_id, "/projects")
  results <- call_asana_api(endpoint, access_token = access_token, archived = archived)
  fix_ids(results$data)

}
