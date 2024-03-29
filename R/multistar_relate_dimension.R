
#' Relate a dimension table to a fact table in a `multistar`
#'
#' Adding a dimension to a `multistar` can only relate to a fact table. You can
#' then relate to other fact tables in the `multistar` using this function. The
#' name of the fact table and its foreign key must be indicated. The referential
#' integrity of the instances of the facts is checked.
#'
#' @param ms A `multistar` object.
#' @param dimension_name A string, name of dimension table.
#' @param fact_name A string, name of fact table.
#' @param fact_key A string, name of the dimension foreign key.
#'
#' @return A `multistar`.
#'
#' @family multistar functions
#'
#' @examples
#'
#' ms <- multistar() |>
#'   add_facts(
#'     fact_name = "mrs_age",
#'     fact_table = mrs_fact_age,
#'     measures = "n_deaths",
#'     nrow_agg = "count"
#'   ) |>
#'   add_facts(
#'     fact_name = "mrs_cause",
#'     fact_table = mrs_fact_cause,
#'     measures = c("pneumonia_and_influenza_deaths", "other_deaths"),
#'     nrow_agg = "nrow_agg"
#'   ) |>
#'   add_dimension(
#'     dimension_name = "where",
#'     dimension_table = mrs_where,
#'     dimension_key = "where_pk",
#'     fact_name = "mrs_age",
#'     fact_key = "where_fk"
#'   ) |>
#'   add_dimension(
#'     dimension_name = "when",
#'     dimension_table = mrs_when,
#'     dimension_key = "when_pk",
#'     fact_name = "mrs_age",
#'     fact_key = "when_fk",
#'     key_as_data = TRUE
#'   ) |>
#'   add_dimension(
#'     dimension_name = "who",
#'     dimension_table = mrs_who,
#'     dimension_key = "who_pk",
#'     fact_name = "mrs_age",
#'     fact_key = "who_fk"
#'   ) |>
#'   relate_dimension(dimension_name = "where",
#'                    fact_name = "mrs_cause",
#'                    fact_key = "where_fk") |>
#'   relate_dimension(dimension_name = "when",
#'                    fact_name = "mrs_cause",
#'                    fact_key = "when_fk")
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
  stopifnot("The name of the dimension must be indicated." = !is.null(dimension_name))
  validate_names(names(ms$dimension), dimension_name, concept = 'dimension name')
  if (is.null(fact_name)) {
    fact_name <- names(ms$fact)[1]
  }
  validate_names(names(ms$fact), fact_name, concept = 'fact name')
  stopifnot("The key of facts must be indicated." = !is.null(fact_key))
  validate_names(names(ms$fact[[fact_name]]), fact_key, concept = 'fact key')
  key <- names(ms$dimension[[dimension_name]])[1]
  validate_names(ms$dimension[[dimension_name]][[key]], unique(ms$fact[[fact_name]][[fact_key]]), concept = 'fact key instance')
  if (key %in% names(ms$fact[[fact_name]])) {
    stop(sprintf("'%s' is already defined in the dimension.", key))
  }
  attr_names <- names(ms$fact[[fact_name]])
  attr_names[which(attr_names == fact_key)] <- key
  names(ms$fact[[fact_name]]) <- attr_names

  attr(ms$fact[[fact_name]], "foreign_keys") <-
    c(attr(ms$fact[[fact_name]], "foreign_keys"), key)

  ms
}
