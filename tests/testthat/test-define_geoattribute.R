context("test define_geoattribute")

test_that("define_geoattribute works", {
  gms <-
    geomultistar(ms = starschemar::ms_mrs_test, geodimension = "where")
  gms <-
    define_geoattribute(
      gms,
      attribute = "city",
      from_layer = usa_cities,
      by = c("city" = "city", "state" = "state")
    )

  expect_equal(
    c(
      names(gms$geodimension$where$city),
      nrow(gms$geodimension$where$city),
      gms$geodimension$where$city$city,
      gms$geodimension$where$city$state
    ),
    c(
      "city",
      "state",
      "geometry",
      "3",
      "Bridgepor",
      "Bridgeport",
      "Tacoma",
      "CT",
      "CT",
      "WA"
    )
  )

  gms <-
    define_geoattribute(gms,
                        attribute = "region",
                        from_attribute = "city",)

  expect_equal(
    c(
      names(gms$geodimension$where$region),
      nrow(gms$geodimension$where$region),
      gms$geodimension$where$region$region
    ),
    c("region", "geometry", "2", "1", "9")
  )

  gms <-
    define_geoattribute(gms,
                        attribute = "all_where",
                        from_layer = usa_nation,)
  expect_equal(class(gms$geodimension$where$all_where$geometry),
               c("sfc_MULTIPOLYGON", "sfc"))
  expect_equal(attr(gms$geodimension$where$all_where, "n_instances"),
               1)

  gms <-
    define_geoattribute(gms,
                        attribute = "all_where",
                        from_attribute = "region",)

  expect_equal(
    c(
      names(gms$geodimension$where$all_where),
      nrow(gms$geodimension$where$all_where),
      gms$geodimension$where$all_where$all_where
    ),
    c("all_where", "geometry", "1", "all_where")
  )
})
