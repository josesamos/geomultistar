
#' Add geometry to a level
#'
#' A level can have several associated geometries (point, polygon or line). Add
#' the geometry of the layer or replace an existing one of the indicated type.
#'
#' @param gl A `geolevel` object.
#' @param layer A `sf` object.
#' @param geometry A string, type of geometry of the layer.
#'
#' @return A `geolevel`.
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
#'     attributes = NULL,
#'     key = c("city", "state"),
#'     geometry = "point"
#'   )
#'
#' # geometry is no checked
#' city <- city %>%
#'   add_geometry(layer = usa_cities,
#'                geometry = "polygon")
#'
#' @export
add_geometry <- function(gl,
                         layer = NULL,
                         geometry = NULL) {
  UseMethod("add_geometry")
}


#' @rdname add_geometry
#' @export
add_geometry.geolevel <- function(gl,
                                  layer = NULL,
                                  geometry = NULL) {
  key <- attr(gl, "key")
  if (is_top_level(gl)) {
    layer <- layer %>%
      tibble::add_column(all_name = "All", .before = 1)
  }

  attributes <- names(layer)
  stopifnot(key %in% attributes)
  stopifnot(length(geometry) == 1)
  stopifnot(geometry %in% c("polygon", "point", "line"))

  layer <- layer %>%
    dplyr::select(tidyselect::all_of(key)) %>%
    dplyr::group_by_at(key) %>%
    dplyr::summarize(.groups = "drop")
  # only the key and geometry

  layer <- gl$data %>%
    dplyr::left_join(layer, by = attr(gl, "key")) %>%
    sf::st_as_sf() %>%
    dplyr::select(tidyselect::all_of(names(gl$data)[1]))

  # only instances with geometry
  layer <- layer[!is.na(sf::st_dimension(layer)), ]

  if (geometry %in% names(gl)) {
    attr(gl, "n_instances_layer") <- nrow(layer)
  } else {
    attr(gl, "n_instances_layer") <- c(nrow(layer), attr(gl, "n_instances_layer"))
  }
  gl[[geometry]] <- layer

  gl
}
