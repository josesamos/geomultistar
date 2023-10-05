context("test get_empty_geoinstances")

#skip("geometry in tibble error")
library(sf) # It has to be included even if it is not used directly.

test_that("get_empty_geoinstances works", {
  gms <-
    geomultistar(ms = starschemar::ms_mrs_test, geodimension = "where")
  gms <-
    define_geoattribute(
      gms,
      attribute = "city",
      from_layer = usa_cities,
      by = c("city" = "city", "state" = "state")
    )

  empty <- get_empty_geoinstances(gms, attribute = "city")

  expect_equal(
    c(names(empty),
      nrow(empty),
      empty$city, empty$state),
    c("city", "state", "geometry", "1", "Bridgepor", "CT")
  )
})
