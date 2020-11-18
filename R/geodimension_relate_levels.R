#' Relate levels in a dimension
#'
#'
#' @param gd A `geodimension` object.
#' @param lower_level_name A string, name of the lower level.
#' @param lower_level_attributes A vector of attribute names.
#' @param upper_level_name A string, name of the upper lever.
#' @param upper_level_attributes A vector of attribute names.
#' @param by_geography A boolean.
#'
#' @return A `geodimension`.
#'
#' @family geodimension functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#'
#' @export
relate_levels <- function(gd,
                          lower_level_name = NULL,
                          lower_level_attributes = NULL,
                          upper_level_name = NULL,
                          upper_level_attributes = NULL,
                          by_geography = FALSE) {
  UseMethod("relate_levels")
}


#' @rdname relate_levels
#' @export
relate_levels.geodimension <- function(gd,
                                       lower_level_name = NULL,
                                       lower_level_attributes = NULL,
                                       upper_level_name = NULL,
                                       upper_level_attributes = NULL,
                                       by_geography = FALSE) {
  stopifnot(lower_level_name %in% names(gd$geolevel))
  stopifnot(upper_level_name %in% names(gd$geolevel))
  lower_level_attributes <- unique(lower_level_attributes)
  upper_level_attributes <- unique(upper_level_attributes)
  stopifnot(length(lower_level_attributes) == length(upper_level_attributes))
  stopifnot(lower_level_attributes %in% attr(gd$geolevel[[lower_level_name]], "attributes"))
  stopifnot(upper_level_attributes %in% attr(gd$geolevel[[upper_level_name]], "attributes"))
  stopifnot(!(upper_level_name %in% names(gd$relation[[lower_level_name]])))

  if (by_geography) {
    stopifnot(is.null(lower_level_attributes) & is.null(upper_level_attributes))
    stopifnot("polygon" %in% names(gd$geolevel[[upper_level_name]]$geometry))
    lower_geom <- names(gd$geolevel[[lower_level_name]]$geometry)
    if ("point" %in% lower_geom) {
      lower_geom <- "point"
    } else if ("line" %in% lower_geom) {
      lower_geom <- "line"
    }
    res <- sf::st_join(gd$geolevel[[lower_level_name]]$geometry[[lower_geom]], gd$geolevel[[upper_level_name]]$geometry[["polygon"]], join = sf::st_within) %>%
      sf::st_drop_geometry()
    names(res) <- c(lower_level_name, upper_level_name)
    gd$relation[[lower_level_name]] <-
      gd$relation[[lower_level_name]] %>%
      dplyr::left_join(res, by = lower_level_name)

  } else if (attr(gd$geolevel[[upper_level_name]], "n_instances_data") == 1) {
    stopifnot(is.null(lower_level_attributes) & is.null(upper_level_attributes))
    gd$relation[[lower_level_name]] <- gd$relation[[lower_level_name]] %>%
      tibble::add_column(!!upper_level_name := gd$relation[[upper_level_name]][[upper_level_name]])
  } else {
    lower_data <-
      gd$geolevel[[lower_level_name]]$data[, c(attr(gd$geolevel[[lower_level_name]], "surrogate_key"),
                                               lower_level_attributes)]
    names(lower_data) <-
      c(attr(gd$geolevel[[lower_level_name]], "surrogate_key"),
        upper_level_attributes)
    upper_data <-
      gd$geolevel[[upper_level_name]]$data[, c(attr(gd$geolevel[[upper_level_name]], "surrogate_key"),
                                               upper_level_attributes)]
    lower_data <- lower_data %>%
      dplyr::left_join(upper_data, by = upper_level_attributes) %>%
      dplyr::select(c(
        attr(gd$geolevel[[lower_level_name]], "surrogate_key"),
        attr(gd$geolevel[[upper_level_name]], "surrogate_key")
      ))
    names(lower_data) <- c(lower_level_name, upper_level_name)

    multiplicity_n_1 <- nrow(lower_data) == nrow(gd$relation[[lower_level_name]])
    stopifnot(multiplicity_n_1)

    gd$relation[[lower_level_name]] <- gd$relation[[lower_level_name]] %>%
      dplyr::left_join(lower_data, by = lower_level_name)

  }
  gd
}


# unrelated instances -----------------------------------------------------

#' Relate levels in a dimension
#'
#'
#' @param gd A `geodimension` object.
#' @param lower_level_name A string, name of the lower level.
#' @param upper_level_name A string, name of the upper lever.
#'
#' @return A `tibble`.
#'
#' @family geodimension functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#'
#' @export
get_unrelated_instances <- function(gd,
                          lower_level_name = NULL,
                          upper_level_name = NULL) {
  UseMethod("get_unrelated_instances")
}


#' @rdname get_unrelated_instances
#' @export
get_unrelated_instances.geodimension <- function(gd,
                                       lower_level_name = NULL,
                                       upper_level_name = NULL) {
  stopifnot(lower_level_name %in% names(gd$geolevel))
  stopifnot(upper_level_name %in% names(gd$geolevel))
  stopifnot(upper_level_name %in% names(gd$relation[[lower_level_name]]))

  unrelated <- gd$relation[[lower_level_name]][[lower_level_name]][is.na(gd$relation[[lower_level_name]][[upper_level_name]])]
  gd$geolevel[[lower_level_name]]$data[unrelated, ]
}

