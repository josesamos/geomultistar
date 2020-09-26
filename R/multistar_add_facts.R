
#' Add a fact table to a `multistar`
#'
#'
#'
#' @param ms A `multistar` object.
#' @param fact_name A string, name of fact table.
#' @param fact_table A `tibble`, fact table.
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
add_facts <- function(ms,
                      fact_name = NULL,
                      fact_table = NULL) {
  UseMethod("add_facts")
}


#' @rdname add_facts
#' @export
add_facts.multistar <- function(ms,
                                fact_name = NULL,
                                fact_table = NULL) {
  stopifnot(!is.null(fact_name))
  stopifnot(!(fact_name %in% names(ms$fact)))
  ms$fact[[fact_name]] <-
    structure(
      fact_table,
      class = unique(append(class(fact_table), "fact_table")),
      name = fact_name,
      foreign_keys = NULL,
      measures = NULL,
      agg_functions = NULL,
      nrow_agg = NULL
    )

  ms
}
