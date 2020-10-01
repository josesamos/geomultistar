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
#' @param top_level A boolean, is the level Top of a dimension.
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
      class = "geolevel"
    )
  }

#' `geolevel` S3 class
#'
#' A `geolevel` object is created. Dimensions that contain geographic
#' information are indicated.
#'
#' @param name A string, level name.
#' @param layer A `sf` object.
#' @param attributes A vector, selected attributes.
#' @param key A vector, attributes that compose the key.
#' @param geometry A string, type of geometry of the layer.
#' @param top_level A boolean, is the level Top of a dimension.
#'
#' @return A `geolevel` object.
#'
#' @family geolevel functions
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
