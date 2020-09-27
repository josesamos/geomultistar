
#' Add a fact table to a `multistar`
#'
#' To add a fact table to a `multistar` object, we must indicate the name that
#' we give to the facts, the `tibble` that contains the data and a vector of
#' attribute names corresponding to the measures.
#'
#' Associated with each measurement, an aggregation function is required, which
#' by default is SUM. It that can be SUM, MAX or MIN. Mean is not considered
#' among the possible aggregation functions: The reason is that calculating the
#' mean by considering subsets of data does not necessarily yield the mean of
#' the total data.
#'
#' An additional measurement, `nrow_agg`, corresponding to the number of
#' aggregated rows is always added which, together with SUM, allows us to obtain
#' the mean if needed. As the value of this parameter, you can specify an
#' attribute of the table or the name that you want to assign to it (if it does
#' not exist, it is added to the table).
#'
#' @param ms A `multistar` object.
#' @param fact_name A string, name of fact table.
#' @param fact_table A `tibble`, fact table.
#' @param measures A vector of measure names.
#' @param agg_functions A vector of aggregation function names. If none is
#'   indicated, the default is SUM. Additionally they can be MAX or MIN.
#' @param nrow_agg A string, measurement name for the number of rows aggregated.
#'   If it does not exist, it is added to the table.
#'
#' @return A `multistar`.
#'
#' @family multistar functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#' ms <- multistar() %>%
#'   add_facts(
#'     fact_name = "mrs_age",
#'     fact_table = mrs_fact_age,
#'     measures = "n_deaths",
#'     nrow_agg = "count"
#'   ) %>%
#'   add_facts(
#'     fact_name = "mrs_cause",
#'     fact_table = mrs_fact_cause,
#'     measures = c("pneumonia_and_influenza_deaths", "other_deaths"),
#'     nrow_agg = "nrow_agg"
#'   )
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
  stopifnot(tibble::is_tibble(fact_table))
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

  measures_type <-
    dplyr::summarise_all(fact_table[, c(measures, nrow_agg)], class)
  for (n in seq_along(measures_type)) {
    type <- measures_type[[n]][1]
    stopifnot(type %in% c("integer", "double", "integer64", "numeric"))
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
