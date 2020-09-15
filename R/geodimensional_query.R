#' geodimensional_query S3 class
#'
#' Internal low-level constructor that creates new objects with the correct
#' structure.
#'
#' @return A `geodimensional_query` object.
#'
#' @keywords internal
new_geodimensional_query <- function(ms = NULL) {
  schema <-
    list(
      fact = NULL,
      dimension = NULL,
      key = NULL,
      input = ms,
      output = NULL
    )

  structure(schema,
            class = c("geodimensional_query", "dimensional_query"))
}


#' `geodimensional_query` S3 class
#'
#' An empty `geodimensional_query` object is created where you can select fact
#' measures, dimension attributes and filter dimension rows.
#'
#' @param ms A `multistar` object.
#'
#' @return A `geodimensional_query` object.
#'
#' @examples
#' library(tidyr)
#' library(starschemar)
#'
#' ms_mrs <- ct_mrs %>%
#'   constellation_as_multistar()
#'
#' dq <- geodimensional_query(ms_mrs)
#'
#' @export
geodimensional_query <- function(ms = NULL) {
  new_geodimensional_query(ms)
}

