
#' Add a dimension table to a `multistar`
#'
#'
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
  stopifnot(!is.null(dimension_name))
  stopifnot(!(dimension_name %in% names(ms$dimension)))
  stopifnot(tibble::is_tibble(dimension_table))
  stopifnot(!is.null(dimension_key))
  stopifnot(dimension_key %in% names(dimension_table))
  stopifnot(length(dimension_table[[dimension_key]]) == length(unique(dimension_table[[dimension_key]])))
  stopifnot(fact_name %in% names(ms$fact))
  if (is.null(fact_name)) {
    fact_name <- names(ms$fact)[1]
  }
  stopifnot(!is.null(fact_key))
  stopifnot(fact_key %in% names(ms$fact[[fact_name]]))
  stopifnot(unique(ms$fact[[fact_name]][[fact_key]]) %in% dimension_table[[dimension_key]])
  key <- sprintf("%s_key", dimension_name)
  stopifnot(!(key %in% names(dimension_table)))
  stopifnot(!(key %in% names(ms$fact[[fact_name]])))

  dimension_table <-
    dplyr::mutate(dimension_table,!!key := dimension_table[[dimension_key]], .before = 1)
  if (!key_as_data) {
    dimension_table <-
      dplyr::select(dimension_table,-(!!dimension_key))
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
