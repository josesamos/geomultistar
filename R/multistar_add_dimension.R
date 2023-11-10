#' Add a dimension table to a `multistar`
#'
#' To add a dimension table to a `multistar` object, we must indicate the name
#' that we give to the dimension, the `tibble` that contains the data and the
#' name of the attribute corresponding to the table primary key.
#'
#' We cannot add a dimension without defining a correspondence with one of the
#' `multistar`'s fact tables. We have to define the name of the fact table and
#' the name of its foreign key. The referential integrity of the instances of
#' the facts is checked.
#'
#' The attribute that is used as the primary key will no longer be accessible
#' for queries (its function is considered to be exclusively related to facts).
#' If you want to use it for queries, it must be explicitly indicated by the
#' boolean parameter `key_as_data`.
#'
#' @param ms A `multistar` object.
#' @param dimension_name A string, name of dimension table.
#' @param dimension_table A `tibble`, dimension table.
#' @param dimension_key A string, name of the dimension primary key.
#' @param fact_name A string, name of fact table.
#' @param fact_key A string, name of the dimension foreign key.
#' @param key_as_data A boolean, define the primary key as an attribute of the
#'   dimension accessible in queries?
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
#'   )
#'
#' @export
add_dimension <- function(ms,
                          dimension_name = NULL,
                          dimension_table = NULL,
                          dimension_key = NULL,
                          fact_name = NULL,
                          fact_key = NULL,
                          key_as_data = FALSE) {
  UseMethod("add_dimension")
}


#' @rdname add_dimension
#' @export
add_dimension.multistar <- function(ms,
                                    dimension_name = NULL,
                                    dimension_table = NULL,
                                    dimension_key = NULL,
                                    fact_name = NULL,
                                    fact_key = NULL,
                                    key_as_data = FALSE) {
  stopifnot("The name of the dimension must be indicated." = !is.null(dimension_name))
  stopifnot("The dimension is already included." = !(dimension_name %in% names(ms$dimension)))
  stopifnot("The dimension table must be a 'tibble'." = tibble::is_tibble(dimension_table))
  stopifnot("The key of the dimension must be indicated." = !is.null(dimension_key))
  validate_names(names(dimension_table), dimension_key, concept = 'dimension key')
  stopifnot("Dimension key values cannot be repeated." = length(dimension_table[[dimension_key]]) == length(unique(dimension_table[[dimension_key]])))
  if (is.null(fact_name)) {
    fact_name <- names(ms$fact)[1]
  }
  validate_names(names(ms$fact), fact_name, concept = 'fact name')
  stopifnot("The key of the fact must be indicated." = !is.null(fact_key))
  validate_names(names(ms$fact[[fact_name]]), fact_key, concept = 'fact key')
  validate_names(dimension_table[[dimension_key]], unique(ms$fact[[fact_name]][[fact_key]]), concept = 'fact key value')

  key <- sprintf("%s_key", dimension_name)
  if (key %in% names(dimension_table)) {
    stop(sprintf("'%s' is already defined in the dimension.", key))
  }
  if (key %in% names(ms$fact[[fact_name]])) {
    stop(sprintf("'%s' is already defined in facts.", key))
  }

  dimension_table <-
    dplyr::mutate(dimension_table,!!key := dimension_table[[dimension_key]], .before = 1)
  if (!key_as_data) {
    dimension_table <-
      dplyr::select(dimension_table,-tidyselect::all_of(!!dimension_key))
  }
  ms$dimension[[dimension_name]] <-
    structure(
      dimension_table,
      class = unique(append(class(dimension_table), "dimension_table")),
      name = dimension_name,
      type = "general",
      role_playing = NULL
    )

  attr_names <- names(ms$fact[[fact_name]])
  attr_names[which(attr_names == fact_key)] <- key
  names(ms$fact[[fact_name]]) <- attr_names

  attr(ms$fact[[fact_name]], "foreign_keys") <-
    c(attr(ms$fact[[fact_name]], "foreign_keys"), key)

  ms
}
