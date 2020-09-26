#' `multistar` S3 class
#'
#' Internal low-level constructor that creates new objects with the correct
#' structure.
#'
#' @param fact_name A string, name of fact table.
#' @param fact_table A `tibble`, fact table.
#'
#' @return A `multistar` object.
#' @keywords internal
new_multistar <-
  function(fact_name = NULL,
           fact_table = NULL) {
    star <-
      list(
        fact = vector("list", length = 0),
        dimension =  vector("list", length = 0)
      )
    stopifnot(!is.null(fact_name))
    star$fact[[fact_name]] <-
      structure(
        fact_table,
        class = unique(append(class(fact_table), "fact_table")),
        name = fact_name,
        foreign_keys = NULL,
        measures = NULL,
        agg_functions = NULL,
        nrow_agg = NULL
      )

    structure(star,
              class = "multistar")
  }

#' `multistar` S3 class
#'
#' Given a name for the facts and the fact table, a `multistar` object is
#' created.
#'
#' @inheritParams new_multistar
#'
#' @return A `multistar` object.
#'
#' @family multistar functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#' library(starschemar)
#'
#'
#' @export
multistar <- function(fact_name = NULL, fact_table = NULL) {
  new_multistar(fact_name, fact_table)
}

