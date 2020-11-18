
#' Add geometry to a level
#'
#' A level can have several associated geometries (point, polygon or line). Add
#' the geometry of the layer or replace an existing one of the indicated type.
#'
#' @param gl A `geolevel` object.
#' @param layer A `sf` object.
#' @param key A vector of string.
#'
#' @return A `geolevel`.
#'
#' @family geodimension functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#'
#' @export
add_geometry <- function(gl,
                         layer = NULL,
                         key = NULL) {
  UseMethod("add_geometry")
}


#' @rdname add_geometry
#' @export
add_geometry.geolevel <- function(gl,
                                  layer = NULL,
                                  key = NULL) {
  geometry <- get_geometry(layer)
  stopifnot(geometry %in% c("polygon", "point", "line"))
  stopifnot(!(geometry %in% names(gl$geometry)))
  level_key <- attr(gl, "key")
  if (is.null(key)) {
    key <- level_key
  }
  stopifnot(length(key) == length(level_key))
  stopifnot(key %in% names(layer))

  layer <- layer %>%
    dplyr::select(tidyselect::all_of(key)) %>%
    dplyr::group_by_at(key) %>%
    dplyr::summarize(.groups = "drop")
  # only the key and geometry
  names_layer <- names(layer)
  names(layer) <- c(level_key, names_layer[length(names_layer)])

  layer <- gl$data %>%
    dplyr::select(tidyselect::all_of(c(attr(gl, "surrogate_key"), level_key))) %>%
    dplyr::left_join(layer, by = level_key) %>%
    sf::st_as_sf() %>%
    dplyr::select(attr(gl, "surrogate_key"))

  # only instances with geometry
  layer <- layer[!is.na(sf::st_dimension(layer)), ]

  gl$geometry[[geometry]] <- layer

  gl
}

# empty geometry ----------------------------------------------------------

#' get empty geometry
#'
#' A level can have several associated geometries (point, polygon or line). Add
#' the geometry of the layer or replace an existing one of the indicated type.
#'
#' @param gl A `geolevel` object.
#' @param geometry A string, type of geometry of the layer.
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
get_empty_geometry_instances <- function(gl,
                         geometry = NULL) {
  UseMethod("get_empty_geometry_instances")
}


#' @rdname get_empty_geometry_instances
#' @export
get_empty_geometry_instances.geolevel <- function(gl,
                                  geometry = NULL) {
  stopifnot(!(geometry %in% names(gl$geometry)))
  if (is.null(geometry)) {
    geometry <- names(gl$geometry)[1]
  }
  gl$data[!(gl$data[[1]] %in% gl$geometry[[geometry]][[1]]), ]
}

