#' Save as geopackage
#'
#' Save the result of a geoquery in a geopackage. The result can be a layer in
#' the form of a flat table or a list consisting of a layer and a description
#' table of the variables.
#'
#' @param sf A `tibble` or a list of `tibble` objects.
#' @param layer_name A string.
#' @param file_name A string.
#' @param filepath A string.
#'
#' @return A `tibble` or a list of `tibble` objects.
#' @export
#'
#' @examples
#'
#' gms <- geomultistar(ms = ms_mrs, geodimension = "where") |>
#'   define_geoattribute(
#'     attribute = "city",
#'     from_layer = usa_cities,
#'     by = c("city" = "city", "state" = "state")
#'   )  |>
#'   define_geoattribute(
#'     attribute = "state",
#'     from_layer = usa_states,
#'     by = c("state" = "state")
#'   ) |>
#'   define_geoattribute(attribute = "region",
#'                       from_attribute = "state") |>
#'   define_geoattribute(attribute = "all_where",
#'                       from_layer = usa_nation)
#'
#' gdq <- dimensional_query(gms) |>
#'   select_dimension(name = "where",
#'                    attributes = c("state", "city")) |>
#'   select_dimension(name = "when",
#'                    attributes = c("when_happened_year", "when_happened_week")) |>
#'   select_fact(
#'     name = "mrs_age",
#'     measures = c("n_deaths")
#'   ) |>
#'   select_fact(name = "mrs_cause",
#'               measures = c("pneumonia_and_influenza_deaths", "other_deaths")) |>
#'   filter_dimension(name = "when", when_happened_week <= "03") |>
#'   filter_dimension(name = "where", state == "MA")
#'
#' sf <- gdq |>
#'   run_geoquery(wider = TRUE)
#'
#' save_as_geopackage(sf, "city", filepath = tempdir())
#'
save_as_geopackage <- function(sf, layer_name, file_name = NULL, filepath = NULL) {
  if (is.null(file_name)) {
    file_name <- layer_name
  }
  if (!is.null(filepath)) {
    filepath <- name_with_nexus(filepath)
  }
  filepath <- paste0(filepath, file_name, ".gpkg")
  if (is.list(sf)) {
    sf::st_write(sf$sf, filepath, layer = layer_name, append = FALSE, delete_dsn = TRUE, driver = 'GPKG')
    if (length(sf) > 1) {
      db <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = filepath)
      RSQLite::dbWriteTable(db, paste(layer_name, "variables", sep = "_"), sf$variables, overwrite = TRUE)
      RSQLite::dbDisconnect(db)
    }
  } else {
    sf::st_write(sf, filepath, layer = layer_name, append = FALSE, delete_dsn = TRUE, driver = 'GPKG')
  }
  sf
}
