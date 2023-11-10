
#' Select fact
#'
#' To define the fact to be consulted, its name is indicated, optionally, a
#' vector of names of selected measures and another of aggregation functions are
#' also indicated.
#'
#' If the name of any measure is not indicated, only the one corresponding to
#' the number of aggregated rows is included, which is always included.
#'
#' If no aggregation function is included, those defined for the measures are
#' considered.
#'
#' @param dq A `dimensional_query` object.
#' @param name A string, name of the fact.
#' @param measures A vector of measure names.
#' @param agg_functions A vector of aggregation function names. If none is
#'   indicated, those defined in the fact table are considered.
#'
#' @return A `dimensional_query` object.
#'
#' @family query functions
#'
#' @examples
#'
#' dq <- dimensional_query(ms_mrs) |>
#'   select_fact(
#'     name = "mrs_age",
#'     measures = c("n_deaths"),
#'     agg_functions = c("MAX")
#'   )
#'
#' dq <- dimensional_query(ms_mrs) |>
#'   select_fact(name = "mrs_age",
#'              measures = c("n_deaths"))
#'
#' dq <- dimensional_query(ms_mrs) |>
#'   select_fact(name = "mrs_age")
#'
#' @export
select_fact <- function(dq,
                       name = NULL,
                       measures = NULL,
                       agg_functions = NULL) {
  UseMethod("select_fact")
}


#' @rdname select_fact
#' @export
select_fact.dimensional_query <- function(dq,
                                         name = NULL,
                                         measures = NULL,
                                         agg_functions = NULL) {
  stopifnot("The name of the fact must be indicated." = !is.null(name))
  validate_names(names(dq$input$fact), name, concept = 'fact name')
  stopifnot("The fact had already been selected." = !(name %in% names(dq$fact)))
  stopifnot("There are repeated measures" = length(measures) == length(unique(measures)))
  for (af in agg_functions) {
    validate_names(c("SUM", "MAX", "MIN"), af, concept = 'aggregation function')
  }
  all_measures <- attr(dq$input$fact[[name]], "measures")
  nrow_agg <- attr(dq$input$fact[[name]], "nrow_agg")
  pos <- which(all_measures == nrow_agg)
  all_measures <- all_measures[-pos]
  all_functions <- attr(dq$input$fact[[name]], "agg_functions")
  all_functions <- all_functions[-pos]
  for (measure in measures) {
    validate_names(all_measures, measure, concept = 'measure')
  }
  if (length(agg_functions) > 0) {
    stopifnot("Measures and aggregation functions do not correspond." = length(measures) == length(agg_functions))
  } else {
    measures <- all_measures[which(measures %in% all_measures)]
    agg_functions <- all_functions[which(measures %in% all_measures)]
  }
  attributes <- c(agg_functions, "SUM")
  names(attributes) <- c(measures, nrow_agg)
  if (is.null(dq$fact)) {
    dq$fact <- list(name = attributes)
    names(dq$fact) <- name
  } else {
    fact_names <- names(dq$fact)
    dq$fact <- c(dq$fact, list(name = attributes))
    names(dq$fact) <- c(fact_names, name)
  }
  dq
}
