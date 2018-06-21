#' List all available projects in Asana
#'
#' @param archived A logical TRUE or FALSE to pull archived or live projects. Defaults to false.
#' @param access_token An Asana access token.
#' @return A list containing a dataframe of all available projects to the Asana user.
#' @references \url{https://asana.com/developers/api-reference/projects#query}
#' @examples
#' \donttest{
#' ## Marked as don't test because an access token is needed
#' get_all_projects()
#' }
#' @export

get_all_projects <- function(archived = FALSE, access_token = Sys.getenv("ASANA_ACCESS_TOKEN")){

  results <- call_asana_api("projects", access_token = access_token, archived = archived)

  results$data <- fix_ids(results$data)

  results

}

#' List a specific workspace's projects in Asana
#'
#' @param workspace_id An Asana workspace ID
#' @param archived A logical TRUE or FALSE to pull archived or live projects. Defaults to false.
#' @param access_token An Asana access token.
#' @return A list containing a dataframe of all projects available in a specific workspace
#' @references \url{https://asana.com/developers/api-reference/projects#query}
#' @examples
#' \donttest{
#' ## Marked as don't test because an access token is needed
#' get_workspace_projects()
#' }
#' @export

get_workspace_projects <- function(workspace_id, archived = FALSE, access_token = Sys.getenv("ASANA_ACCESS_TOKEN")){

  endpoint <- paste0("workspaces/", workspace_id, "/projects")

  results <- call_asana_api(endpoint, access_token = access_token, archived = archived)

  results$data <- fix_ids(results$data)

  results

}
