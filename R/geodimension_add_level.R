
#' Relate a dimension table to a fact table in a `geodimension`
#'
#' Adding a dimension to a `geodimension` can only relate to a fact table. You can
#' then relate to other fact tables in the `geodimension` using this function. The
#' name of the fact table and its foreign key must be indicated. The referential
#' integrity of the instances of the facts is checked.
#'
#' @param gd A `geodimension` object.
#' @param level A `geolevel`, level to add to the dimension.
#' @param lower_level_name A string, name of the lower level.
#' @param upper_level_name A string, name of the upper lever.
#'
#' @return A `geodimension`.
#'
#' @family geodimension functions
#' @seealso
#'
#' @examples
#'
#' @export
add_level <- function(gd,
                      level = NULL,
                      lower_level_name = NULL,
                      upper_level_name = NULL) {
  UseMethod("add_level")
}


#' @rdname add_level
#' @export
add_level.geodimension <- function(gd,
                                   level = NULL,
                                   lower_level_name = NULL,
                                   upper_level_name = NULL) {
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

  if (upper_level_name != attr(gd, "top_level")) {
    related_upper_and_lower_level <-
      upper_level_name %in% attr(gd[[lower_level_name]], "parent_level")
    stopifnot(related_upper_and_lower_level)
  }

  # delete old lower-upper relation
  i <-
    which(attr(gd[[lower_level_name]], "parent_level") == upper_level_name)
  if (length(i) > 0) {
    gd[[lower_level_name]]$data <-
      gd[[lower_level_name]]$data %>%
      dplyr::select(-(!!(attr(gd[[lower_level_name]], "parent_key")[i])))
    attr(gd[[lower_level_name]], "parent_key") <-
      attr(gd[[lower_level_name]], "parent_key")[-i]
    attr(gd[[lower_level_name]], "parent_level") <-
      attr(gd[[lower_level_name]], "parent_level")[-i]
  }

  # relate new level with upper level
  if (upper_level_name == attr(gd, "top_level")) {
    level$data[[attr(gd[[upper_level_name]], "surrogate_key")]] <-
      gd[[upper_level_name]]$data[[attr(gd[[upper_level_name]], "surrogate_key")]]
  } else {
    stopifnot(attr(gd[[upper_level_name]], "key") %in% names(level$data))

    attributes <-
      c(attr(gd[[upper_level_name]], "key"), attr(gd[[upper_level_name]], "surrogate_key"))
    upper_level_data <-
      gd[[upper_level_name]]$data %>%
      dplyr::select(tidyselect::all_of(attributes))

    level$data <-
      level$data %>%
      dplyr::left_join(upper_level_data, by = attr(gd[[upper_level_name]], "key"))
  }
  attr(level, "parent_level") <- upper_level_name
  attr(level, "parent_key") <-
    attr(gd[[upper_level_name]], "surrogate_key")


  # relate lower level to new level
  attributes <-
    c(attr(level, "key"), attr(level, "surrogate_key"))
  level_data <-
    level$data %>%
    dplyr::select(tidyselect::all_of(attributes))

  gd[[lower_level_name]]$data <-
    gd[[lower_level_name]]$data %>%
    dplyr::left_join(level_data, by = attr(level, "key"))

  attr(gd[[lower_level_name]], "parent_level") <-
    c(attr(gd[[lower_level_name]], "parent_level"), attr(level, "name"))
  attr(gd[[lower_level_name]], "parent_key") <-
    c(attr(gd[[lower_level_name]], "parent_key"), attr(level, "surrogate_key"))

  gd[[attr(level, "name")]] <- level

  gd
}
