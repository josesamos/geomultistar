context("test dimensional_query_save")

test_that("dimensional_query_save works", {
  gms <- geomultistar(ms = ms_mrs, geodimension = "where") |>
    define_geoattribute(
      attribute = "city",
      from_layer = usa_cities,
      by = c("city" = "city", "state" = "state")
    )  |>
    define_geoattribute(
      attribute = "state",
      from_layer = usa_states,
      by = c("state" = "state")
    ) |>
    define_geoattribute(attribute = "region",
                        from_attribute = "state") |>
    define_geoattribute(attribute = "all_where",
                        from_layer = usa_nation)

  gdq <- dimensional_query(gms) |>
    select_dimension(name = "where",
                     attributes = c("state", "city")) |>
    select_dimension(
      name = "when",
      attributes = c("when_happened_year", "when_happened_week")
    ) |>
    select_fact(name = "mrs_age",
                measures = c("n_deaths")) |>
    select_fact(
      name = "mrs_cause",
      measures = c("pneumonia_and_influenza_deaths", "other_deaths")
    ) |>
    filter_dimension(name = "when", when_happened_week <= "03") |>
    filter_dimension(name = "where", state == "MA")

  sf <- gdq |>
    run_geoquery(wider = TRUE)

  filepath <-  tempdir()
  s <- save_as_geopackage(sf, "city", filepath = filepath)


  l <- sf::st_layers(paste0(filepath, "/city", ".gpkg"))


  #  in upcoming sf, st_layers() returns an object of class c("sf_layers", "data.frame"); this breaks a test in geomultistar:
  # expect_equal(class(l),
  #              "sf_layers")


  expect_equal(l$name,
               c("city", "city_variables"))

})
