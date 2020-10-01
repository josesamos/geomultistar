context("test check_referential_integrity")

test_that("check_referential_integrity works", {

  city <-
    geolevel(
      name = "city",
      layer = usa_cities,
      key = c("city", "state"),
      geometry = "point"
    )

  state <-
    geolevel(
      name = "state",
      layer = usa_states,
      key = c("state"),
      geometry = "polygon"
    )

  all <-
    geolevel(layer = usa_nation,
             geometry = "polygon",
             top_level = TRUE)

  ft <- check_referential_integrity(city, state)

  expect_equal(ft,
               structure(
                 list(state_key = integer(0), state = character(0)),
                 row.names = integer(0),
                 class = c("tbl_df",
                           "tbl", "data.frame")
               ))

  ft <- check_referential_integrity(city, all)

  expect_equal(ft,
               structure(
                 list(a = character(0)),
                 row.names = integer(0),
                 class = c("tbl_df",
                           "tbl", "data.frame")
               ))


  city$data$state[1] <- "00"
  ft <- check_referential_integrity(city, state)
  expect_equal(ft,
               structure(
                 list(state_key = NA_integer_, state = "00"),
                 row.names = c(NA,-1L),
                 class = c("tbl_df", "tbl", "data.frame")
               ))

})
