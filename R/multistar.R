#' `multistar` S3 class
#'
#' Internal low-level constructor that creates new objects with the correct
#' structure.
#'
#' @return A `multistar` object.
#' @keywords internal
new_multistar <-
  function() {
    star <-
      list(
        fact = vector("list", length = 0),
        dimension =  vector("list", length = 0)
      )

    structure(star,
              class = "multistar")
  }

#' `multistar` S3 class
#'
#' Creates an empty `multistar` object.
#'
#' @return A `multistar` object.
#'
#' @family multistar functions
#' @seealso
#'
#' @examples
#'
#' ms <- multistar()
#'
#' @export
multistar <- function() {
  new_multistar()
}

