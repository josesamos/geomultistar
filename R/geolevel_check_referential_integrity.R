
#' Check the referential integrity of a level with respect to another
#'
#' Before relating one level to another in a dimension, we must check its
#' referential integrity: if all the instances of the lower level are going to
#' be able to relate to some instance of the higher level.
#'
#' Returns a table with the instances that cannot be related. It can be selected
#' to include only the common attributes with the top level or all attributes on
#' the level.
#'
#' @param level A `geolevel`, level to check.
#' @param upper_level A `geolevel`, reference level.
#' @param include_all_attributes A boolean, include all level attributes.
#'
#' @return A `tibble`.
#'
#' @family geodimension functions
#' @seealso
#'
#' @examples
#'
#' city <-
#'   geolevel(
#'     name = "city",
#'     layer = usa_cities,
#'     key = c("city", "state"),
#'     geometry = "point"
#'   )
#'
#' state <-
#'   geolevel(
#'     name = "state",
#'     layer = usa_states,
#'     key = c("state"),
#'     geometry = "polygon"
#'   )
#'
#' ft <- check_referential_integrity(city, state)
#'
#' @export
check_referential_integrity <- function(level = NULL,
                                        upper_level = NULL,
                                        include_all_attributes = FALSE) {
  UseMethod("check_referential_integrity")
}


#' @rdname check_referential_integrity
#' @export
check_referential_integrity.geolevel <- function(level = NULL,
                                                 upper_level = NULL,
                                                 include_all_attributes = FALSE) {
  if (is_top_level(upper_level)) {
    ft <- tibble::tibble(a = character())
  } else {
    key <- attr(upper_level, "key")
    surrogate_key <- attr(upper_level, "surrogate_key")
    attributes <- attr(level, "attributes")
    attributes <- attributes[attributes %in% attr(upper_level, "attributes")]
    if (include_all_attributes) {
      attributes <- unique(c(surrogate_key, key, attributes, attr(level, "attributes")))
    } else {
      attributes <- unique(c(surrogate_key, key, attributes))
    }

    level <- relate_to_upper_level(level, upper_level)

    ft <- unique(tibble::tibble(data.frame(level$data[is.na(level$data[[surrogate_key]]), attributes])))
  }
  ft
}
