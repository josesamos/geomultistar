
#' Relate a dimension table to a fact table in a `multistar`
#'
#'
#'
#' @param ms A `multistar` object.
#' @param dimension_name A string, name of dimension table.
#' @param fact_name A string, name of fact table.
#' @param fact_key A string, name of the dimension foreign key.
#'
#' @return A `tibble`.
#'
#' @family multistar functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#'
#' @export
relate_dimension <- function(ms,
                             dimension_name = NULL,
                             fact_name = NULL,
                             fact_key = NULL) {
  UseMethod("relate_dimension")
}


#' @rdname relate_dimension
#' @export
relate_dimension.multistar <- function(ms,
                                       dimension_name = NULL,
                                       fact_name = NULL,
                                       fact_key = NULL) {
  stopifnot(!is.null(dimension_name))
  stopifnot(dimension_name %in% names(ms$dimension))
  stopifnot(fact_name %in% names(ms$fact))
  if (is.null(fact_name)) {
    fact_name <- names(ms$fact)[1]
  }
  stopifnot(!is.null(fact_key))
  stopifnot(fact_key %in% names(ms$fact[[fact_name]]))
  key <- names(ms$dimension[[dimension_name]])[1]
  stopifnot(unique(ms$fact[[fact_name]][[fact_key]]) %in% ms$dimension[[dimension_name]][[key]])
  stopifnot(!(key %in% names(ms$fact[[fact_name]])))

  attr_names <- names(ms$fact[[fact_name]])
  attr_names[which(attr_names == fact_key)] <- key
  names(ms$fact[[fact_name]]) <- attr_names

  ms
}
