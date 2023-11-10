context("test run_geoquery")

test_that("run_geoquery works", {
  gms <- geomultistar(ms = ms_mrs_test, geodimension = "where")
  gms <-
    define_geoattribute(
      gms,
      attribute = "city",
      from_layer = usa_cities,
      by = c("city" = "city", "state" = "state")
    )
  gms <-
    define_geoattribute(
      gms,
      attribute = "state",
      from_layer = usa_states,
      by = c("state" = "state")
    )
  gms <-
    define_geoattribute(
      gms,
      attribute = "region",
      from_layer = usa_regions,
      by = c("region" = "geo_id")
    )
  gms <-
    define_geoattribute(
      gms,
      attribute = "all_where",
      from_layer = usa_nation,
    )

  gdq <- dimensional_query(gms) |>
    select_dimension(name = "where",
                                  attributes = c("city", "state", "region")) |>
    select_dimension(name = "when",
                                  attributes = c("year", "week")) |>
    select_fact(
      name = "mrs_age",
      measures = c("deaths"),
      agg_functions = c("SUM")
    ) |>
    select_fact(name = "mrs_cause",
                             measures = c("pneumonia_and_influenza_deaths", "other_deaths")) |>
    filter_dimension(name = "when", week <= "03") |>
    filter_dimension(name = "where", city == "Bridgeport")

  gr1 <- gdq |>
    run_geoquery(attribute = "city")

  expect_equal(
    c(names(gr1),
      nrow(gr1),
      gr1$year, gr1$week, gr1$city),
    c(
      "year",
      "week",
      "region",
      "state",
      "city",
      "deaths",
      "nrow_agg",
      "mrs_cause_pneumonia_and_influenza_deaths",
      "mrs_cause_other_deaths",
      "mrs_cause_nrow_agg",
      "geometry",
      "2",
      "1962",
      "1962",
      "01",
      "02",
      "Bridgeport",
      "Bridgeport"
    )
  )

  gr3 <- gdq |>
    run_geoquery()

  expect_equal(
    c(names(gr3),
      nrow(gr3),
      gr3$year, gr3$week, gr3$city),
    c(
      "year",
      "week",
      "region",
      "state",
      "city",
      "deaths",
      "nrow_agg",
      "mrs_cause_pneumonia_and_influenza_deaths",
      "mrs_cause_other_deaths",
      "mrs_cause_nrow_agg",
      "geometry",
      "2",
      "1962",
      "1962",
      "01",
      "02",
      "Bridgeport",
      "Bridgeport"
    )
  )

  gr4 <- gdq |>
    run_geoquery(wider = TRUE)

  expect_equal(
    c(
      names(gr4),
      names(gr4$sf),
      names(gr4$variables),
      nrow(gr4$sf),
      nrow(gr4$variables),
      gr4$sf$city,
      gr4$variables$id_variable,
      gr4$variables$measure
    ),
    c(
      "sf",
      "variables",
      "fid",
      "year",
      "region",
      "state",
      "city",
      "deaths_01",
      "deaths_02",
      "nrow_agg_01",
      "nrow_agg_02",
      "mrs_cause_pneumonia_and_influenza_deaths_01",
      "mrs_cause_pneumonia_and_influenza_deaths_02",
      "mrs_cause_other_deaths_01",
      "mrs_cause_other_deaths_02",
      "mrs_cause_nrow_agg_01",
      "mrs_cause_nrow_agg_02",
      "geometry",
      "id_variable",
      "measure",
      "week",
      "1",
      "10",
      "Bridgeport",
      "deaths_01",
      "deaths_02",
      "nrow_agg_01",
      "nrow_agg_02",
      "mrs_cause_pneumonia_and_influenza_deaths_01",
      "mrs_cause_pneumonia_and_influenza_deaths_02",
      "mrs_cause_other_deaths_01",
      "mrs_cause_other_deaths_02",
      "mrs_cause_nrow_agg_01",
      "mrs_cause_nrow_agg_02",
      "deaths",
      "deaths",
      "nrow_agg",
      "nrow_agg",
      "mrs_cause_pneumonia_and_influenza_deaths",
      "mrs_cause_pneumonia_and_influenza_deaths",
      "mrs_cause_other_deaths",
      "mrs_cause_other_deaths",
      "mrs_cause_nrow_agg",
      "mrs_cause_nrow_agg"
    )
  )

  gdq <- dimensional_query(gms) |>
    # select_dimension(name = "where",
    #                               attributes = c("city", "state", "region")) |>
    select_dimension(name = "when",
                                  attributes = c("year", "week")) |>
    select_fact(
      name = "mrs_age",
      measures = c("deaths"),
      agg_functions = c("SUM")
    ) |>
    select_fact(name = "mrs_cause",
                             measures = c("pneumonia_and_influenza_deaths", "other_deaths")) |>
    filter_dimension(name = "when", week <= "03") |>
    filter_dimension(name = "where", city == "Bridgeport")

  gr5 <- gdq |>
    run_geoquery(wider = TRUE)

  expect_equal(
    c(
      names(gr5),
      names(gr5$sf),
      names(gr5$variables),
      nrow(gr5$sf),
      nrow(gr5$variables),
      gr5$sf$fid,
      gr5$variables$id_variable,
      gr5$variables$measure
    ),
    c("sf", "variables", "fid", "all_where", "year", "deaths_01",
      "deaths_02", "nrow_agg_01", "nrow_agg_02", "mrs_cause_pneumonia_and_influenza_deaths_01",
      "mrs_cause_pneumonia_and_influenza_deaths_02", "mrs_cause_other_deaths_01",
      "mrs_cause_other_deaths_02", "mrs_cause_nrow_agg_01", "mrs_cause_nrow_agg_02",
      "geometry", "id_variable", "measure", "week", "1", "10", "1",
      "deaths_01", "deaths_02", "nrow_agg_01", "nrow_agg_02", "mrs_cause_pneumonia_and_influenza_deaths_01",
      "mrs_cause_pneumonia_and_influenza_deaths_02", "mrs_cause_other_deaths_01",
      "mrs_cause_other_deaths_02", "mrs_cause_nrow_agg_01", "mrs_cause_nrow_agg_02",
      "deaths", "deaths", "nrow_agg", "nrow_agg", "mrs_cause_pneumonia_and_influenza_deaths",
      "mrs_cause_pneumonia_and_influenza_deaths", "mrs_cause_other_deaths",
      "mrs_cause_other_deaths", "mrs_cause_nrow_agg", "mrs_cause_nrow_agg"
    )
  )

})
