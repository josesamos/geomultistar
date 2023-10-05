context("test run_geoquery")

# skip("geometry in tibble error")
library(sf) # It has to be included even if it is not used directly.

test_that("run_geoquery works", {
  gms <- geomultistar(ms = starschemar::ms_mrs_test, geodimension = "where")
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

  gdq <- starschemar::dimensional_query(gms) |>
    starschemar::select_dimension(name = "where",
                                  attributes = c("city", "state", "region")) |>
    starschemar::select_dimension(name = "when",
                                  attributes = c("year", "week")) |>
    starschemar::select_fact(
      name = "mrs_age",
      measures = c("deaths"),
      agg_functions = c("SUM")
    ) |>
    starschemar::select_fact(name = "mrs_cause",
                             measures = c("pneumonia_and_influenza_deaths", "other_deaths")) |>
    starschemar::filter_dimension(name = "when", week <= "03") |>
    starschemar::filter_dimension(name = "where", city == "Bridgeport")

  gr1 <- gdq |>
    run_geoquery(attribute = "city")

  expect_equal(gr1,
               structure(
                 list(
                   year = c("1962", "1962"),
                   week = c("01", "02"),
                   region = c("1", "1"),
                   state = c("CT", "CT"),
                   city = c("Bridgeport",
                            "Bridgeport"),
                   deaths = c(46L, 43L),
                   nrow_agg = 3:4,
                   pneumonia_and_influenza_deaths = 3:2,
                   other_deaths = c(43L, 41L),
                   mrs_cause_nrow_agg = c(1L, 1L),
                   geometry = structure(
                     list(structure(
                       c(-73.2048348, 41.1670412), class = c("XY", "POINT", "sfg")
                     ), structure(
                       c(-73.2048348,
                         41.1670412), class = c("XY", "POINT", "sfg")
                     )),
                     class = c("sfc_POINT",
                               "sfc"),
                     precision = 0,
                     bbox = structure(
                       c(
                         xmin = -73.2048348,
                         ymin = 41.1670412,
                         xmax = -73.2048348,
                         ymax = 41.1670412
                       ),
                       class = "bbox"
                     ),
                     crs = structure(
                       list(input = "NAD83", wkt = "GEOGCRS[\"NAD83\",\n    DATUM[\"North American Datum 1983\",\n        ELLIPSOID[\"GRS 1980\",6378137,298.257222101,\n            LENGTHUNIT[\"metre\",1]]],\n    PRIMEM[\"Greenwich\",0,\n        ANGLEUNIT[\"degree\",0.0174532925199433]],\n    CS[ellipsoidal,2],\n        AXIS[\"latitude\",north,\n            ORDER[1],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        AXIS[\"longitude\",east,\n            ORDER[2],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n    ID[\"EPSG\",4269]]"),
                       class = "crs"
                     ),
                     n_empty = 0L
                   )
                 ),
                 row.names = c(NA,-2L),
                 class = c("sf", "tbl_df", "tbl", "data.frame"),
                 sf_column = "geometry",
                 agr = structure(
                   c(
                     year = NA_integer_,
                     week = NA_integer_,
                     region = NA_integer_,
                     state = NA_integer_,
                     city = NA_integer_,
                     deaths = NA_integer_,
                     nrow_agg = NA_integer_,
                     pneumonia_and_influenza_deaths = NA_integer_,
                     other_deaths = NA_integer_,
                     mrs_cause_nrow_agg = NA_integer_
                   ),
                   class = "factor",
                   .Label = c("constant",
                              "aggregate", "identity")
                 )
               ))

  gr3 <- gdq |>
    run_geoquery()

  expect_equal(sf::st_drop_geometry(gr3),
               structure(
                 list(
                   year = c("1962", "1962"),
                   week = c("01", "02"),
                   region = c("1", "1"),
                   state = c("CT", "CT"),
                   city = c("Bridgeport",
                            "Bridgeport"),
                   deaths = c(46L, 43L),
                   nrow_agg = 3:4,
                   pneumonia_and_influenza_deaths = 3:2,
                   other_deaths = c(43L, 41L),
                   mrs_cause_nrow_agg = c(1L, 1L)
                 ),
                 row.names = 1:2,
                 class = c("tbl_df", "tbl", "data.frame")
               ))

  gr4 <- gdq |>
    run_geoquery(wider = TRUE)

  expect_equal(gr4,
               list(
                 sf = structure(
                   list(
                     fid = 1L,
                     year = "1962",
                     region = "1",
                     state = "CT",
                     city = "Bridgeport",
                     deaths_01 = 46L,
                     deaths_02 = 43L,
                     nrow_agg_01 = 3L,
                     nrow_agg_02 = 4L,
                     pneumonia_and_influenza_deaths_01 = 3L,
                     pneumonia_and_influenza_deaths_02 = 2L,
                     other_deaths_01 = 43L,
                     other_deaths_02 = 41L,
                     mrs_cause_nrow_agg_01 = 1L,
                     mrs_cause_nrow_agg_02 = 1L,
                     geometry = structure(
                       list(structure(
                         c(-73.2048348, 41.1670412), class = c("XY", "POINT", "sfg")
                       )),
                       class = c("sfc_POINT",
                                 "sfc"),
                       precision = 0,
                       bbox = structure(
                         c(
                           xmin = -73.2048348,
                           ymin = 41.1670412,
                           xmax = -73.2048348,
                           ymax = 41.1670412
                         ),
                         class = "bbox"
                       ),
                       crs = structure(
                         list(input = "NAD83", wkt = "GEOGCRS[\"NAD83\",\n    DATUM[\"North American Datum 1983\",\n        ELLIPSOID[\"GRS 1980\",6378137,298.257222101,\n            LENGTHUNIT[\"metre\",1]]],\n    PRIMEM[\"Greenwich\",0,\n        ANGLEUNIT[\"degree\",0.0174532925199433]],\n    CS[ellipsoidal,2],\n        AXIS[\"latitude\",north,\n            ORDER[1],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        AXIS[\"longitude\",east,\n            ORDER[2],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n    ID[\"EPSG\",4269]]"),
                         class = "crs"
                       ),
                       n_empty = 0L
                     )
                   ),
                   row.names = c(NA,-1L),
                   class = c("sf", "tbl_df", "tbl", "data.frame"),
                   sf_column = "geometry",
                   agr = structure(
                     c(
                       fid = NA_integer_,
                       year = NA_integer_,
                       region = NA_integer_,
                       state = NA_integer_,
                       city = NA_integer_,
                       deaths_01 = NA_integer_,
                       deaths_02 = NA_integer_,
                       nrow_agg_01 = NA_integer_,
                       nrow_agg_02 = NA_integer_,
                       pneumonia_and_influenza_deaths_01 = NA_integer_,
                       pneumonia_and_influenza_deaths_02 = NA_integer_,
                       other_deaths_01 = NA_integer_,
                       other_deaths_02 = NA_integer_,
                       mrs_cause_nrow_agg_01 = NA_integer_,
                       mrs_cause_nrow_agg_02 = NA_integer_
                     ),
                     class = "factor",
                     .Label = c("constant",
                                "aggregate", "identity")
                   )
                 ),
                 variables = structure(
                   list(
                     id_variable = c(
                       "deaths_01",
                       "deaths_02",
                       "nrow_agg_01",
                       "nrow_agg_02",
                       "pneumonia_and_influenza_deaths_01",
                       "pneumonia_and_influenza_deaths_02",
                       "other_deaths_01",
                       "other_deaths_02",
                       "mrs_cause_nrow_agg_01",
                       "mrs_cause_nrow_agg_02"
                     ),
                     measure = c(
                       "deaths",
                       "deaths",
                       "nrow_agg",
                       "nrow_agg",
                       "pneumonia_and_influenza_deaths",
                       "pneumonia_and_influenza_deaths",
                       "other_deaths",
                       "other_deaths",
                       "mrs_cause_nrow_agg",
                       "mrs_cause_nrow_agg"
                     ),
                     week = c("01", "02",
                              "01", "02", "01", "02", "01", "02", "01", "02")
                   ),
                   row.names = c(NA,-10L),
                   name = "mrs_age",
                   foreign_keys = c("when_key", "where_key"),
                   measures = c(
                     "deaths",
                     "nrow_agg",
                     "pneumonia_and_influenza_deaths",
                     "other_deaths",
                     "mrs_cause_nrow_agg"
                   ),
                   agg_functions = c(
                     deaths = "SUM",
                     nrow_agg = "SUM",
                     pneumonia_and_influenza_deaths = "SUM",
                     other_deaths = "SUM",
                     mrs_cause_nrow_agg = "SUM"
                   ),
                   nrow_agg = "nrow_agg",
                   class = c("tbl_df",
                             "tbl", "data.frame")
                 )
               ))

  gdq <- starschemar::dimensional_query(gms) |>
    # starschemar::select_dimension(name = "where",
    #                               attributes = c("city", "state", "region")) |>
    starschemar::select_dimension(name = "when",
                                  attributes = c("year", "week")) |>
    starschemar::select_fact(
      name = "mrs_age",
      measures = c("deaths"),
      agg_functions = c("SUM")
    ) |>
    starschemar::select_fact(name = "mrs_cause",
                             measures = c("pneumonia_and_influenza_deaths", "other_deaths")) |>
    starschemar::filter_dimension(name = "when", week <= "03") |>
    starschemar::filter_dimension(name = "where", city == "Bridgeport")

  gr5 <- gdq |>
    run_geoquery(wider = TRUE)

  expect_equal(sf::st_drop_geometry(gr5$sf),
               structure(
                 list(
                   fid = 1L,
                   all_where = "all_where",
                   year = "1962",
                   deaths_01 = 46L,
                   deaths_02 = 43L,
                   nrow_agg_01 = 3L,
                   nrow_agg_02 = 4L,
                   pneumonia_and_influenza_deaths_01 = 3L,
                   pneumonia_and_influenza_deaths_02 = 2L,
                   other_deaths_01 = 43L,
                   other_deaths_02 = 41L,
                   mrs_cause_nrow_agg_01 = 1L,
                   mrs_cause_nrow_agg_02 = 1L
                 ),
                 row.names = 1L,
                 class = c("tbl_df",
                           "tbl", "data.frame")
               ))

  expect_equal(
    gr5$variables,
    structure(
      list(
        id_variable = c(
          "deaths_01",
          "deaths_02",
          "nrow_agg_01",
          "nrow_agg_02",
          "pneumonia_and_influenza_deaths_01",
          "pneumonia_and_influenza_deaths_02",
          "other_deaths_01",
          "other_deaths_02",
          "mrs_cause_nrow_agg_01",
          "mrs_cause_nrow_agg_02"
        ),
        measure = c(
          "deaths",
          "deaths",
          "nrow_agg",
          "nrow_agg",
          "pneumonia_and_influenza_deaths",
          "pneumonia_and_influenza_deaths",
          "other_deaths",
          "other_deaths",
          "mrs_cause_nrow_agg",
          "mrs_cause_nrow_agg"
        ),
        week = c("01", "02", "01", "02", "01", "02", "01", "02", "01",
                 "02")
      ),
      row.names = c(NA,-10L),
      name = "mrs_age",
      foreign_keys = "when_key",
      measures = c(
        "deaths",
        "nrow_agg",
        "pneumonia_and_influenza_deaths",
        "other_deaths",
        "mrs_cause_nrow_agg"
      ),
      agg_functions = c(
        deaths = "SUM",
        nrow_agg = "SUM",
        pneumonia_and_influenza_deaths = "SUM",
        other_deaths = "SUM",
        mrs_cause_nrow_agg = "SUM"
      ),
      nrow_agg = "nrow_agg",
      class = c("tbl_df",
                "tbl", "data.frame")
    )
  )

})
