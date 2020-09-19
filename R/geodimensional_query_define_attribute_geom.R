
#' Define geometric attribute
#'
#'
#' @param gdq A `geodimensional_query` object.
#' @param attribute A string, attribute name.
#' @param layer A `sf` object
#' @param by a vector of correspondence of attributes of the dimension with the
#'   `sf` structure.
#' @param from_attribute A string, attribute name.
#'
#' @return A `geodimensional_query` object.
#'
#' @family query functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#'
#' @export
define_attribute_geom <- function(gdq,
                                  attribute = NULL,
                                  layer = NULL,
                                  by = NULL,
                                  from_attribute = NULL) {
  UseMethod("define_attribute_geom")
}


#' @rdname define_attribute_geom
#' @export
define_attribute_geom.geodimensional_query <-
  function(gdq,
           attribute = NULL,
           layer = NULL,
           by = NULL,
           from_attribute = NULL) {
    dimension <- names(gdq$geodimension)[1]
    stopifnot(attribute %in% names(gdq$geodimension[[dimension]]))
    if (!is.null(from_attribute)) {
      gdq <-
        define_attribute_geom_from_attribute(gdq, dimension, attribute, from_attribute)
    } else {
      gdq <-
        define_attribute_geom_from_layer(gdq, dimension, attribute, layer, by)
    }
    gdq
  }



#' Define one attribute from another
#'
#' Define one attribute from another.
#'
#' @param gdq A `geodimensional_query` object.
#' @param dimension A string, dimension name.
#' @param attribute A string, attribute name.
#' @param from_attribute A string, attribute name.
#'
#' @return A `geodimensional_query` object.
#'
#' @keywords internal
define_attribute_geom_from_attribute <- function(gdq,
                                                 dimension = NULL,
                                                 attribute = NULL,
                                                 from_attribute = NULL) {
  stopifnot(from_attribute %in% names(gdq$geodimension[[dimension]]))
  geom <- gdq$geodimension[[dimension]][[from_attribute]]
  from_attribute_geom_is_defined <- !is.null(geom)
  stopifnot(from_attribute_geom_is_defined)

  if (attribute == sprintf("all_%s", dimension)) {
    gdq$geodimension[[dimension]][[attribute]] <-
      as.data.frame(geom) %>%
      dplyr::mutate(geom_key = 0, .before = from_attribute) %>%
      sf::st_as_sf() %>%
      dplyr::group_by(geom_key) %>%
      dplyr::summarize(.groups = "drop")
  } else {
    key <- sprintf("%s_key", dimension)
    layer <- gdq$input$dimension[[dimension]][, c(key, attribute)] %>%
      dplyr::left_join(geom, by = key)

    from <- length(unique(layer[[from_attribute]]))
    new <- length(unique(layer[[attribute]]))
    from_attribute_is_more_detailed <- (from >= new)
    stopifnot(from_attribute_is_more_detailed)

    layer <- layer %>%
      sf::st_as_sf() %>%
      dplyr::group_by_at(attribute) %>%
      dplyr::summarize(.groups = "drop")

    gdq$geodimension[[dimension]][[attribute]] <-
      dplyr::left_join(gdq$input$dimension[[dimension]][, c(key, attribute)], layer, by = attribute) %>%
      sf::st_as_sf()
  }
  gdq
}


#' Define an attribute from a layer
#'
#' Define an attribute from a layer.
#'
#' @param gdq A `geodimensional_query` object.
#' @param dimension A string, dimension name.
#' @param attribute A string, attribute name.
#' @param layer A `sf` object
#' @param by a vector of correspondence of attributes of the dimension with the
#'   `sf` structure.
#'
#' @return A `geodimensional_query` object.
#'
#' @keywords internal
define_attribute_geom_from_layer <- function(gdq,
                                        dimension = NULL,
                                        attribute = NULL,
                                        layer = NULL,
                                        by = NULL) {
  if (attribute == sprintf("all_%s", dimension)) {
    geometry_level_all_length_is_1 <- (length(layer[[1]]) == 1)
    stopifnot(geometry_level_all_length_is_1)
    gdq$geodimension[[dimension]][[attribute]] <-
      tibble::tibble(geom_key = 0,
                     geometry = sf::st_geometry(layer)) %>%
      sf::st_as_sf()
  } else {
    key <- sprintf("%s_key", dimension)
    geom <-
      dplyr::left_join(gdq$input$dimension[[dimension]], layer, by = by) %>%
      sf::st_as_sf() %>%
      dplyr::select(key)

    relation_1_to_1_with_geometry <-
      length(gdq$input$dimension[[dimension]][[1]]) == length(geom[[1]])
    stopifnot(relation_1_to_1_with_geometry)

    gdq$geodimension[[dimension]][[attribute]] <-
      gdq$input$dimension[[dimension]][, c(key, attribute)] %>%
      dplyr::left_join(geom, by = key) %>%
      sf::st_as_sf()
  }
  gdq
}
