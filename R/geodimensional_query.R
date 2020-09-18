#' geodimensional_query S3 class
#'
#' Internal low-level constructor that creates new objects with the correct
#' structure.
#'
#' @return A `geodimensional_query` object.
#'
#' @keywords internal
new_geodimensional_query <- function(ms = NULL, geodimension = NULL) {
  schema <-
    list(
      geodimension = NULL,
      fact = NULL,
      dimension = NULL,
      key = NULL,
      input = ms,
      output = NULL
    )

  stopifnot(geodimension %in% names(ms$dimension))
  for (name in geodimension) {
    if (is.null(schema$geodimension)) {
      schema$geodimension <- list(name = NULL)
      names(schema$geodimension) <- name
    } else {
      dim_names <- names(schema$geodimension)
      schema$geodimension <- c(schema$geodimension, list(name = NULL))
      names(schema$geodimension) <- c(dim_names, name)
    }
  }
  for (dimension in names(schema$geodimension)) {
    for (name in names(ms$dimension[[dimension]])[-1]) {
      if (is.null(schema$geodimension[[dimension]])) {
        schema$geodimension[[dimension]] <- list(name = NULL)
        names(schema$geodimension[[dimension]]) <- name
      } else {
        dim_names <- names(schema$geodimension[[dimension]])
        schema$geodimension[[dimension]] <- c(schema$geodimension[[dimension]], list(name = NULL))
        names(schema$geodimension[[dimension]]) <- c(dim_names, name)
      }
    }
  }

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
geodimensional_query <- function(ms = NULL, geodimension = NULL) {
  new_geodimensional_query(ms, geodimension)
}

