
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
