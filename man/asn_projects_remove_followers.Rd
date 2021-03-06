% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/projects.R
\name{asn_projects_remove_followers}
\alias{asn_projects_remove_followers}
\title{Removes the specified list of users from following the project, this will not affect project membership status.
Returns the updated project record.}
\usage{
asn_projects_remove_followers(project, ..., options = list())
}
\arguments{
\item{project}{{Id} The project to remove followers from.#'}

\item{...}{{Object} Data for the request
followers : {Array} An array of followers to remove from the project.}
}
\description{
Removes the specified list of users from following the project, this will not affect project membership status.
Returns the updated project record.
}
\seealso{
Other projects: \code{\link{asn_projects_add_custom_field_setting}},
  \code{\link{asn_projects_add_followers}},
  \code{\link{asn_projects_add_members}},
  \code{\link{asn_projects_create_in_team}},
  \code{\link{asn_projects_create_in_workspace}},
  \code{\link{asn_projects_create}},
  \code{\link{asn_projects_delete}},
  \code{\link{asn_projects_find_all}},
  \code{\link{asn_projects_find_by_id}},
  \code{\link{asn_projects_find_by_team}},
  \code{\link{asn_projects_find_by_workspace}},
  \code{\link{asn_projects_remove_custom_field_setting}},
  \code{\link{asn_projects_remove_members}},
  \code{\link{asn_projects_tasks}},
  \code{\link{asn_projects_update}},
  \code{\link{asn_projects}}
}
\concept{projects}
