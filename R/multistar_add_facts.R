
#' Add a fact table to a `multistar`
#'
#'
#'
#' @param ms A `multistar` object.
#' @param fact_name A string, name of fact table.
#' @param fact_table A `tibble`, fact table.
#' @param measures A vector of measure names.
#' @param agg_functions A vector of aggregation function names. If none is
#'   indicated, the default is SUM. Additionally they can be MAX or MIN.
#' @param nrow_agg A string, measurement name for the number of rows aggregated.
#'   If it does not exist, it is added.
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
                      fact_table = NULL,
                      measures = NULL,
                      agg_functions = NULL,
                      nrow_agg = "nrow_agg") {
  UseMethod("add_facts")
}


#' @rdname add_facts
#' @export
add_facts.multistar <- function(ms,
                                fact_name = NULL,
                                fact_table = NULL,
                                measures = NULL,
                                agg_functions = NULL,
                                nrow_agg = "nrow_agg") {
  stopifnot(!is.null(fact_name))
  stopifnot(!(fact_name %in% names(ms$fact)))

  if (is.null(agg_functions)) {
    agg_functions <-  rep("SUM", length(measures))
  }
  stopifnot(length(measures) == length(agg_functions))
  for (af in agg_functions) {
    stopifnot(af %in% c("SUM", "MAX", "MIN"))
  }
  stopifnot(length(c(measures, nrow_agg)) == length(unique(c(measures, nrow_agg))))
  attributes_defined <- names(fact_table)
  for (measure in measures) {
    stopifnot(measure %in% attributes_defined)
  }
  if (!(nrow_agg %in% attributes_defined)) {
    fact_table <- dplyr::mutate(fact_table, !!nrow_agg := as.integer(1))
  }

  ms$fact[[fact_name]] <-
    structure(
      fact_table,
      class = unique(append(class(fact_table), "fact_table")),
      name = fact_name,
      foreign_keys = NULL,
      measures = c(measures, nrow_agg),
      agg_functions = c(agg_functions, "SUM"),
      nrow_agg = nrow_agg
    )

  ms
}
