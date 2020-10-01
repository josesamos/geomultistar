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

    stopifnot(attr(top_level, "n_instances") == 1)

    botton_level$data[[attr(top_level, "surrogate_key")]] <-
      top_level$data[[attr(top_level, "surrogate_key")]]
    attr(botton_level, "parent_level") <- attr(top_level, "name")
    attr(botton_level, "parent_key") <-
      attr(top_level, "surrogate_key")

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
#' A `geodimension` object is created. Dimensions that contain geographic
#' information are indicated.
#'
#' @inheritParams new_geodimension
#'
#' @return A `geodimension` object.
#'
#' @family geolevel functions
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
