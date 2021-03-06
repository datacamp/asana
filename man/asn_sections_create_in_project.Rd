% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sections.R
\name{asn_sections_create_in_project}
\alias{asn_sections_create_in_project}
\title{Creates a new section in a project.}
\usage{
asn_sections_create_in_project(project, ..., options = list())
}
\arguments{
\item{project}{{Id} The project to create the section in#'}

\item{...}{{Object} Data for the request
name : {String} The text to be displayed as the section name. This cannot be an empty string.}
}
\description{
Returns the full record of the newly created section.
}
\seealso{
Other sections: \code{\link{asn_sections_delete}},
  \code{\link{asn_sections_find_by_id}},
  \code{\link{asn_sections_find_by_project}},
  \code{\link{asn_sections_insert_in_project}},
  \code{\link{asn_sections_update}},
  \code{\link{asn_sections}}
}
\concept{sections}
