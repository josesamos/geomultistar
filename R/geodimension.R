#' `geodimension` S3 class
#'
#' Internal low-level constructor that creates new objects with the correct
#' structure.
#'
#' @param name A string, name of the dimension.
#' @param botton_level A `geolevel`, level with the finest granularity.
#' @param top_level A `geolevel`, level *All*, with a single instance.
#'
#' @return A `geodimension` object.
#'
#' @keywords internal
new_geodimension <-
  function(name = NULL,
           botton_level = NULL,
           top_level = NULL) {

    stopifnot(is_top_level(top_level))

    botton_level <- relate_to_top_level(botton_level, top_level)

    geodimension <- list()
    geodimension[[attr(botton_level, "name")]] <- botton_level
    geodimension[[attr(top_level, "name")]] <- top_level

    structure(
      geodimension,
      name = name,
      botton_level = attr(botton_level, "name"),
      top_level = attr(top_level, "name"),
      class = "geodimension"
    )
  }

#' `geodimension` S3 class
#'
#' A `geodimension` object is created. In addition to the name of the dimension,
#' the lowest and highest level of the dimension are indicated.
#'
#' @inheritParams new_geodimension
#'
#' @return A `geodimension` object.
#'
#' @family geodimension functions
#' @seealso
#'
#' @examples
#'
#'
#' @export
geodimension <- function(name = NULL,
                         botton_level = NULL,
                         top_level = NULL) {
  new_geodimension(name, botton_level, top_level)
}


