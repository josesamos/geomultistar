#' `geolevel` S3 class
#'
#' Internal low-level constructor that creates new objects with the correct
#' structure.
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @importFrom rlang :=
#'
#' @param name A string, level name.
#' @param layer A `sf` object.
#' @param attributes A vector, selected attributes.
#' @param key A vector, attributes that compose the key.
#' @param geometry A string, type of geometry of the layer.
#' @param top_level A boolean, is the level *Top* of a dimension.
#'
#' @return A `geolevel` object.
#'
#' @keywords internal
new_geolevel <-
  function(name = NULL,
           layer = NULL,
           attributes = NULL,
           key = NULL,
           geometry = NULL,
           top_level = FALSE) {

    stopifnot(length(geometry) == 1)
    stopifnot(geometry %in% c("polygon", "point", "line"))

    if (top_level) {
      name <- "all"
      layer <- layer %>%
        tibble::add_column(all_name = "All", .before = 1)
      attributes = "all_name"
      key = "all_name"
    }

    stopifnot(!is.null(name))
    data <- tibble::tibble((sf::st_drop_geometry(layer)))
    stopifnot(length(attributes) == length(unique(attributes)))
    stopifnot(attributes %in% names(data))
    if (is.null(attributes)) {
      attributes <-  names(data)
    }
    stopifnot(!is.null(key))
    stopifnot(length(key) == length(unique(key)))
    stopifnot(key %in% attributes)

    data <- data %>%
      dplyr::select(tidyselect::all_of(attributes)) %>%
      dplyr::group_by_at(attributes) %>%
      dplyr::summarize(.groups = "drop")

    data_key <- data %>%
      dplyr::select(tidyselect::all_of(key)) %>%
      dplyr::group_by_at(key) %>%
      dplyr::summarize(.groups = "drop")

    is_a_valid_key <- nrow(data) == nrow(data_key)
    stopifnot(is_a_valid_key)

    surrogate_key <- sprintf("%s_key", name)
    data_key <- data_key %>%
      tibble::add_column(!!surrogate_key := 1:nrow(data_key), .before = 1)

    data <- data_key %>%
      dplyr::left_join(data, by = key)

    layer <- layer %>%
      dplyr::select(tidyselect::all_of(key)) %>%
      dplyr::group_by_at(key) %>%
      dplyr::summarize(.groups = "drop")

    layer <- data_key %>%
      dplyr::left_join(layer, by = key) %>%
      sf::st_as_sf() %>%
      dplyr::select(tidyselect::all_of(names(data_key)[1]))

    # only instances with geometry
    layer <- layer[!is.na(sf::st_dimension(layer)), ]

    geolevel <- list(data = data,
                     geometry = layer)
    names(geolevel) <- c("data", geometry)

    structure(
      geolevel,
      name = name,
      attributes = attributes,
      key = key,
      surrogate_key = surrogate_key,
      n_instances_layer = nrow(layer),
      n_instances_data = nrow(data),
      parent_level = NULL,
      parent_key = NULL,
      class = "geolevel"
    )
  }

#' `geolevel` S3 class
#'
#' A `geolevel` object is created from a given geographic layer. The attributes
#' of the layer to be included in the level are indicated, and the subset of
#' these that make up the natural key.
#'
#' A level can have several associated geometries (point, polygon or line), the
#' geometry of the layer is indicated.
#'
#' The *Top* level of any dimension is a special case since it has only one
#' object. This situation is indicated by a boolean parameter.
#'
#' @param name A string, level name.
#' @param layer A `sf` object.
#' @param attributes A vector, selected attributes.
#' @param key A vector, attributes that compose the key.
#' @param geometry A string, type of geometry of the layer.
#' @param top_level A boolean, is the level *Top* of a dimension.
#'
#' @return A `geolevel` object.
#'
#' @family geodimension functions
#' @seealso
#'
#' @examples
#'
#'
#' @export
geolevel <-
  function(name = NULL,
           layer = NULL,
           attributes = NULL,
           key = NULL,
           geometry = NULL,
           top_level = FALSE) {
    new_geolevel(name, layer, attributes, key, geometry, top_level)
  }


#' Is it *Top* level of dimension?
#'
#' @param level A `geolevel`.
#'
#' @return A boolean.
#'
#' @keywords internal
is_top_level <- function(level) {
  (attr(level, "name") == "all" & attr(level, "n_instances_layer") == 1)
}


#' Are levels related?
#'
#' A level is always related to the *Top* level.
#'
#' A level can be added between any level and the *Top* level, otherwise, lower
#' and upper levels must be related.
#'
#' @param level A `geolevel`.
#' @param upper_level A `geolevel`.
#'
#' @return A boolean.
#'
#' @keywords internal
are_levels_related <- function(level, upper_level) {
  if (is_top_level(upper_level)) {
    res <- TRUE
  } else {
    res <- (attr(upper_level, "name") %in% attr(level, "parent_level"))
  }
  res
}


#' Relate level to *Top* level
#'
#' @param level A `geolevel`.
#' @param top_level A `geolevel`, level *All*, with a single instance.
#'
#' @return level A `geolevel`.
#'
#' @keywords internal
relate_to_top_level <- function(level, top_level) {
  tl_surrogate_key <- attr(top_level, "surrogate_key")
  level$data[[tl_surrogate_key]] <-
    top_level$data[[tl_surrogate_key]]
  attr(level, "parent_level") <- attr(top_level, "name")
  attr(level, "parent_key") <- tl_surrogate_key
  level
}


#' De-relate level to upper level.
#'
#' @param level A `geolevel`.
#' @param upper_level_name A string, name of the upper lever.
#'
#' @return level A `geolevel`.
#'
#' @keywords internal
derelate_to_upper_level <- function(level, upper_level_name) {
    i <- which(attr(level, "parent_level") == upper_level_name)
    if (length(i) > 0) {
      level$data <- level$data %>%
        dplyr::select(-(!!(attr(level, "parent_key")[i])))
      attr(level, "parent_key") <- attr(level, "parent_key")[-i]
      attr(level, "parent_level") <- attr(level, "parent_level")[-i]
    }
    level
  }


#' Relate level to upper level.
#'
#' @param level A `geolevel`.
#' @param upper_level A `geolevel`.
#'
#' @return level A `geolevel`.
#'
#' @keywords internal
relate_to_upper_level <- function(level, upper_level) {
  if (is_top_level(upper_level)) {
    level <- relate_to_top_level(level, upper_level)
  } else {
    key <- attr(upper_level, "key")
    level_contains_upper_level_key <- (key %in% names(level$data))
    stopifnot(level_contains_upper_level_key)

    attributes <- c(key, attr(upper_level, "surrogate_key"))
    upper_level_data <- upper_level$data %>%
      dplyr::select(tidyselect::all_of(attributes))

    level$data <- level$data %>%
      dplyr::left_join(upper_level_data, by = key)
    attr(level, "parent_level") <-
      c(attr(level, "parent_level"), attr(upper_level, "name"))
    attr(level, "parent_key") <-
      c(attr(level, "parent_key"), attr(upper_level, "surrogate_key"))
  }
  level
}
