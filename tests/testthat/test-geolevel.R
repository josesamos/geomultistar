context("test geolevel")

test_that("geolevel works", {
  city <-
    geolevel(
      name = "city",
      layer = usa_cities,
      attributes = c("city", "state"),
      key = c("city", "state"),
      geometry = "point"
    )

  expect_equal(
    attributes(city),
    list(
      names = c("data", "point"),
      name = "city",
      attributes = c("city",
                     "state"),
      key = c("city", "state"),
      surrogate_key = "city_key",
      n_instances_layer = 117L,
      n_instances_data = 117L,
      class = "geolevel"
    )
  )

  expect_equal(names(city$data),
               c("city_key", "city", "state"))

  all <-
    geolevel(layer = usa_nation,
             geometry = "polygon",
             top_level = TRUE)

  expect_equal(
    attributes(all),
    list(
      names = c("data", "polygon"),
      name = "all",
      attributes = "all_name",
      key = "all_name",
      surrogate_key = "all_key",
      n_instances_layer = 1L,
      n_instances_data = 1L,
      class = "geolevel"
    )
  )

  expect_equal(names(all$data),
               c("all_key", "all_name"))
})
