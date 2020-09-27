
#' Define geographic attributes
#'
#' Defines a geographic attributes in two possible ways: Associates the
#' instances of attributes of the geographic dimension with the instances of a
#' geographic layer or defines it from the geometry of another previously
#' defined geographic attribute. Multiple attributes can be specified in the
#' `attribute` parameter.
#'
#' If defined from a layer (`from_layer` parameter), additionally the attributes
#' used for the join between the tables (dimension and layer tables) must be
#' indicated (`by` parameter).
#'
#' If defined from another attribute, it should have a finer granularity, to
#' obtain the result by grouping its instances.
#'
#' If no value is indicated in the `attribute` parameter, it is defined for all
#' those attributes of the dimension that do not have any previous definition,
#' they are obtained from the attribute indicated in the `from_attribute`
#' parameter.
#'
#' @param gms A `geomultistar` object.
#' @param dimension A string, dimension name.
#' @param attribute A vector, attribute names.
#' @param from_layer A `sf` object.
#' @param by a vector of correspondence of attributes of the dimension with the
#'   `sf` layer structure.
#' @param from_attribute A string, attribute name.
#'
#' @return A `geomultistar` object.
#'
#' @family geo functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#' library(starschemar)
#' library(sf)
#'
#' gms <- geomultistar(ms = ms_mrs, geodimension = "where") %>%
#'   define_geoattribute(
#'     attribute = "city",
#'     from_layer = usa_cities,
#'     by = c("city" = "city", "state" = "state")
#'   )
#'
#' gms <- gms %>%
#'   define_geoattribute(attribute = c("region", "all_where"),
#'                       from_attribute = "city")
#'
#' gms <- gms %>%
#'   define_geoattribute(from_attribute = "city")
#'
#' gms <- gms %>%
#'   define_geoattribute(attribute = "all_where",
#'                       from_layer = usa_nation)
#'
#' @export
define_geoattribute <- function(gms,
                                dimension = NULL,
                                attribute = NULL,
                                from_layer = NULL,
                                by = NULL,
                                from_attribute = NULL) {
  UseMethod("define_geoattribute")
}


#' @rdname define_geoattribute
#' @export
define_geoattribute.geomultistar <-
  function(gms,
           dimension = NULL,
           attribute = NULL,
           from_layer = NULL,
           by = NULL,
           from_attribute = NULL) {
    if (is.null(dimension)) {
      dimension <- names(gms$geodimension)[1]
    }
    is_geographic_dimension <-
      dimension %in% names(gms$geodimension)
    stopifnot(is_geographic_dimension)
    is_geographic_attribute <-
      attribute %in% names(gms$geodimension[[dimension]])
    stopifnot(is_geographic_attribute)
    if (is.null(attribute)) {
      # default
      stopifnot(!is.null(from_attribute))
      for (attribute in names(gms$geodimension[[dimension]])) {
        if (is.null(gms$geodimension[[dimension]][[attribute]])) {
          gms <-
            define_geoattribute_from_attribute(gms, dimension, attribute, from_attribute)
        }
      }
    } else {
      for (att in attribute) {
        if (!is.null(from_attribute)) {
          gms <-
            define_geoattribute_from_attribute(gms, dimension, att, from_attribute)
        } else {
          gms <-
            define_geoattribute_from_layer(gms, dimension, att, from_layer, by)
        }
      }
    }
    gms
  }



#' Define a geoattribute from another
#'
#' Define a geoattribute from another.
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @importFrom rlang :=
#' @param gms A `geomultistar` object.
#' @param dimension A string, dimension name.
#' @param attribute A string, attribute name.
#' @param from_attribute A string, attribute name.
#'
#' @return A `geomultistar` object.
#'
#' @keywords internal
define_geoattribute_from_attribute <- function(gms,
                                               dimension = NULL,
                                               attribute = NULL,
                                               from_attribute = NULL) {
  stopifnot(!is.null(from_attribute))
  is_geographic_from_attribute <- from_attribute %in% names(gms$geodimension[[dimension]])
  stopifnot(is_geographic_from_attribute)
  geom <- gms$geodimension[[dimension]][[from_attribute]]
  from_attribute_geom_is_defined <- !is.null(geom)
  stopifnot(from_attribute_geom_is_defined)

  if (attribute == sprintf("all_%s", dimension)) {
    gms$geodimension[[dimension]][[attribute]] <-
      as.data.frame(geom) %>%
      dplyr::mutate(!!attribute := 0, .before = from_attribute) %>%
      sf::st_as_sf() %>%
      dplyr::group_by_at(attribute) %>%
      dplyr::summarize(.groups = "drop")
    attr(gms$geodimension[[dimension]][[attribute]], 'n_instances') <- 1
  } else {
    key <- sprintf("%s_key", dimension)
    layer <-
      gms$dimension[[dimension]][, c(key, attribute)] %>%
      dplyr::left_join(geom, by = key)

    from <- length(unique(layer[[from_attribute]]))
    new <- length(unique(layer[[attribute]]))
    from_attribute_is_more_detailed <- (from >= new)
    stopifnot(from_attribute_is_more_detailed)

    layer <- layer %>%
      sf::st_as_sf() %>%
      dplyr::group_by_at(attribute) %>%
      dplyr::summarize(.groups = "drop")

    gms$geodimension[[dimension]][[attribute]] <-
      dplyr::left_join(gms$dimension[[dimension]][, c(key, attribute)], layer, by = attribute) %>%
      sf::st_as_sf()
    attr(gms$geodimension[[dimension]][[attribute]], 'n_instances') <-
      length(unique(gms$geodimension[[dimension]][[attribute]][[attribute]]))
  }
  gms
}


#' Define an attribute from a layer
#'
#' Define an attribute from a layer.
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @importFrom rlang :=
#' @param gms A `geomultistar` object.
#' @param dimension A string, dimension name.
#' @param attribute A string, attribute name.
#' @param layer A `sf` object
#' @param by a vector of correspondence of attributes of the dimension with the
#'   `sf` structure.
#'
#' @return A `geomultistar` object.
#'
#' @keywords internal
define_geoattribute_from_layer <- function(gms,
                                           dimension = NULL,
                                           attribute = NULL,
                                           from_layer = NULL,
                                           by = NULL) {
  stopifnot(!is.null(from_layer))
  if (attribute == sprintf("all_%s", dimension)) {
    geometry_level_all_length_is_1 <- (length(from_layer[[1]]) == 1)
    stopifnot(geometry_level_all_length_is_1)
    gms$geodimension[[dimension]][[attribute]] <-
      tibble::tibble(!!attribute := 0,
                     geometry = sf::st_geometry(from_layer)) %>%
      sf::st_as_sf()
    attr(gms$geodimension[[dimension]][[attribute]], 'n_instances') <- 1
  } else {
    stopifnot(!is.null(by))
    key <- sprintf("%s_key", dimension)
    geom <-
      dplyr::left_join(gms$dimension[[dimension]], from_layer, by = by) %>%
      sf::st_as_sf() %>%
      dplyr::select(key)

    relation_1_to_1_with_geometry <-
      length(gms$dimension[[dimension]][[1]]) == length(geom[[1]])
    stopifnot(relation_1_to_1_with_geometry)

    gms$geodimension[[dimension]][[attribute]] <-
      gms$dimension[[dimension]][, c(key, attribute)] %>%
      dplyr::left_join(geom, by = key) %>%
      sf::st_as_sf()
    attr(gms$geodimension[[dimension]][[attribute]], 'n_instances') <-
      length(unique(gms$dimension[[dimension]][, names(by)])[[1]])
  }
  gms
}
