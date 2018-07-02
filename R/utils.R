
#' Fix IDs
#'
#' Asana stores IDs as integers. Many of these integers are greater than the
#' values that R can store, \code{.Machine$integer.max}. That causes
#' \code{jsonlite} to convert them to \code{numeric}. This function undo that.
#' @param id A numeric vector.
#' @return A character vector.
#' @seealso \code{\link[base]{format}}
#' @examples
#' id <- jsonlite::fromJSON("1234567890123450")
#' fix_ids(id)
#' @noRd
fix_ids <- function(id) {
  format(id, digits = 22)
}

#' Fix Archived
#'
#' Asana projects can be fetched for either all, live, or archived only.
#' We require a function that changes NA to NULL to make this clean in our calls.
#' @param archived A logical or NA
#' @return A logical or NA
#' @seealso \code{\link[base]{format}}
#' @examples
#' fix_archived(NA)
#' @noRd
#' @importFrom assertive.types assert_is_a_bool
fix_archived <- function(archived){

  assert_is_a_bool(archived)

  if(is.na(archived)){

    return(NULL)

  }

  archived

}
