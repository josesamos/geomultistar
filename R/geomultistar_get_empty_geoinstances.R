
#' Get empty instances of a geographic attribute
#'
#' Gets the instances of the given geographic attribute that do not have a
#' geometry associated with them.
#'
#' @param gms A `geomultistar` object.
#' @param dimension A string, dimension name.
#' @param attribute A string, attribute name.
#'
#' @return A `sf` object.
#'
#' @family geo functions
#'
#' @examples
#' library(starschemar)
#' library(sf)
#'
#' gms <- geomultistar(ms = ms_mrs, geodimension = "where") |>
#'   define_geoattribute(
#'     attribute = "city",
#'     from_layer = usa_cities,
#'     by = c("city" = "city", "state" = "state")
#'   )
#'
#'   empty <- gms |>
#'      get_empty_geoinstances(attribute = "city")
#'
#' @export
get_empty_geoinstances <- function(gms,
                                dimension = NULL,
                                attribute = NULL) {
  UseMethod("get_empty_geoinstances")
}


#' @rdname get_empty_geoinstances
#' @export
get_empty_geoinstances.geomultistar <-
  function(gms,
           dimension = NULL,
           attribute = NULL) {
    if (is.null(dimension)) {
      dimension <- names(gms$geodimension)[1]
    }
    is_geographic_dimension <-
      dimension %in% names(gms$geodimension)
    stopifnot(is_geographic_dimension)
    is_geographic_attribute <-
      attribute %in% names(gms$geodimension[[dimension]])
    stopifnot(is_geographic_attribute)
    the_attribute_is_defined <- !is.null(gms$geodimension[[dimension]][[attribute]])
    stopifnot(the_attribute_is_defined)

    gms$geodimension[[dimension]][[attribute]][is.na(sf::st_dimension(gms$geodimension[[dimension]][[attribute]])), ]
  }
