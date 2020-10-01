
#' Relate a dimension table to a fact table in a `geolevel`
#'
#' Adding a dimension to a `geolevel` can only relate to a fact table. You can
#' then relate to other fact tables in the `geolevel` using this function. The
#' name of the fact table and its foreign key must be indicated. The referential
#' integrity of the instances of the facts is checked.
#'
#' @param gl A `geolevel` object.
#' @param layer A `sf` object.
#' @param geometry A string, type of geometry of the layer.
#'
#' @return A `geolevel`.
#'
#' @family geolevel functions
#' @seealso
#'
#' @examples
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
  if (attr(gl, "name") == "all" & attr(gl, "n_instances_layer") == 1) {
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
    attr(gl, "n_instances") <- nrow(layer)
  } else {
    attr(gl, "n_instances") <- c(nrow(layer), attr(gl, "n_instances"))
  }
  gl[[geometry]] <- layer

  gl
}
