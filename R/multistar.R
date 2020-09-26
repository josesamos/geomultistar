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
    star$fact[[fact_name]] <- fact_table

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
#' @family geo functions
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

