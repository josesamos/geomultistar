context("test add_level")

test_that("add_level works", {

  city <-
    geolevel(
      name = "city",
      layer = usa_cities,
      key = c("city", "state"),
      geometry = "point"
    )

  all <-
    geolevel(layer = usa_nation,
             geometry = "polygon",
             top_level = TRUE)

  usa_city <-
    geodimension(name = "usa_city",
                 botton_level = city,
                 top_level = all)

  state <-
    geolevel(
      name = "state",
      layer = usa_states,
      key = c("state"),
      geometry = "polygon"
    )

  usa_city <- usa_city %>%
    add_level(level = state)


  expect_equal(
    attributes(usa_city),
    list(
      names = c("city", "all", "state"),
      name = "usa_city",
      botton_level = "city",
      top_level = "all",
      class = "geodimension"
    )
  )

  expect_equal(
    attributes(usa_city$city),
    list(
      names = c("data", "point"),
      name = "city",
      attributes = c(
        "gnis_id",
        "ansi_code",
        "city",
        "state",
        "county",
        "latitude",
        "longitude",
        "elev_m"
      ),
      key = c("city", "state"),
      surrogate_key = "city_key",
      n_instances_layer = 117L,
      n_instances_data = 117L,
      class = "geolevel",
      parent_level = "state",
      parent_key = "state_key"
    )
  )

  expect_equal(
    names(usa_city$city$data),
    c(
      "city_key",
      "city",
      "state",
      "gnis_id",
      "ansi_code",
      "county",
      "latitude",
      "longitude",
      "elev_m",
      "state_key"
    )
  )

  expect_equal(
    attributes(usa_city$state),
    list(
      names = c("data", "polygon"),
      name = "state",
      attributes = c("geo_id",
                     "state", "state_name"),
      key = "state",
      surrogate_key = "state_key",
      n_instances_layer = 37L,
      n_instances_data = 37L,
      class = "geolevel",
      parent_level = "all",
      parent_key = "all_key"
    )
  )

  expect_equal(
    names(usa_city$state$data),
    c("state_key", "state", "geo_id", "state_name", "all_key")
  )

})
