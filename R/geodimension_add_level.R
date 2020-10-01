
#' Add a level to a dimension
#'
#' A new level must always lie between two levels of the dimension. Given the
#' level and the names of the two levels between which it is inserted, it adds
#' it to the dimension.
#'
#' If any of them is not indicated, by default it is considered the lowest level
#' and the highest level of the dimension.
#'
#' It offers the possibility of automatically removing the attributes included
#' in a level from its lower level. Different attributes must have different
#' names in the dimension levels.
#'
#' We can define that the level is only included if there is referential
#' integrity between the instances of the levels to be related.
#'
#' @param gd A `geodimension` object.
#' @param level A `geolevel`, level to add to the dimension.
#' @param lower_level_name A string, name of the lower level.
#' @param upper_level_name A string, name of the upper lever.
#' @param normalize_levels A boolean, remove the attributes included in a level
#'   from its lower level.
#' @param check_referential_integrity A boolean, check for unrelated instances.
#'
#' @return A `geodimension`.
#'
#' @family geodimension functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#' city <-
#'   geolevel(
#'     name = "city",
#'     layer = usa_cities,
#'     key = c("city", "state"),
#'     geometry = "point"
#'   )
#'
#' all <-
#'   geolevel(
#'     layer = usa_nation,
#'     geometry = "polygon",
#'     top_level = TRUE
#'   )
#'
#' usa_city <-
#'   geodimension(name = "usa_city",
#'                botton_level = city,
#'                top_level = all)
#'
#' state <-
#'   geolevel(
#'     name = "state",
#'     layer = usa_states,
#'     key = c("state"),
#'     geometry = "polygon"
#'   )
#'
#' usa_city <- usa_city %>%
#'   add_level(level = state)
#'
#' @export
add_level <- function(gd,
                      level = NULL,
                      lower_level_name = NULL,
                      upper_level_name = NULL,
                      normalize_levels = TRUE,
                      check_referential_integrity = TRUE) {
  UseMethod("add_level")
}


#' @rdname add_level
#' @export
add_level.geodimension <- function(gd,
                                   level = NULL,
                                   lower_level_name = NULL,
                                   upper_level_name = NULL,
                                   normalize_levels = TRUE,
                                   check_referential_integrity = TRUE) {
  if (is.null(lower_level_name)) {
    lower_level_name <- attr(gd, "botton_level")
  }
  if (is.null(upper_level_name)) {
    upper_level_name <- attr(gd, "top_level")
  }
  level_names <- names(gd)
  stopifnot(lower_level_name %in% level_names)
  stopifnot(upper_level_name %in% level_names)
  stopifnot(!(attr(level, "name") %in% level_names))
  stopifnot(are_levels_related(gd[[lower_level_name]], gd[[upper_level_name]]))

  gd[[lower_level_name]] <- derelate_to_upper_level(gd[[lower_level_name]], upper_level_name)

  if(check_referential_integrity) {
    ft <- check_referential_integrity(level, gd[[upper_level_name]])
    referential_integrity_is_met <- (nrow(ft) == 0)
    stopifnot(referential_integrity_is_met)

    ft <- check_referential_integrity(gd[[lower_level_name]], level)
    referential_integrity_is_met <- (nrow(ft) == 0)
    stopifnot(referential_integrity_is_met)
  }

  level <- relate_to_upper_level(level, gd[[upper_level_name]])
  gd[[lower_level_name]] <- relate_to_upper_level(gd[[lower_level_name]], level)

  if(normalize_levels) {
    gd[[lower_level_name]] <- normalize_level(gd[[lower_level_name]], level)
    level <- normalize_level(level, gd[[upper_level_name]])
  }

  gd[[attr(level, "name")]] <- level
  gd
}
