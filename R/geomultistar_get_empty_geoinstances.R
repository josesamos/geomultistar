
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
    validate_names(names(gms$geodimension), dimension, concept = 'geographic dimension')
    validate_names(names(gms$geodimension[[dimension]]), attribute, concept = 'geographic attribute')
    stopifnot("The attribute is defined." = !is.null(gms$geodimension[[dimension]][[attribute]]))
    gms$geodimension[[dimension]][[attribute]][is.na(sf::st_dimension(gms$geodimension[[dimension]][[attribute]])), ]
  }
