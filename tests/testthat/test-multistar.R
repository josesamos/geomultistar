context("test multistar")

test_that("multistar works", {
  ms <- multistar()

  st <- st_mrs_age_test
  fl <-  NULL
  dl <-  NULL
  commondim <-  NULL
  fl_names <- names(fl)
  fl <- c(fl, list(st$fact[[1]]))
  names(fl) <- c(fl_names, attr(st$fact[[1]], "name"))

  names <- names(st$dimension)
  dimensions <- st$dimension[names]
  for (n in names) {
    dimensions[[n]] <- st$dimension[[n]]
  }
  dim <- dimensions
  dl_names <- names(dl)
  for (d in seq_along(dim)) {
    name_dim <- attr(dim[[d]], "name")
    if (!(name_dim %in% commondim)) {
      dl <- c(dl, list(dim[[d]]))
      dl_names <- c(dl_names, name_dim)
    }
  }
  names(dl) <- dl_names
  nms <- new_multistar(fl, dl)

  ft <- ms_mrs |>
    multistar_as_flat_table(fact = "mrs_age")


  expect_equal(ms, structure(list(
    fact = list(), dimension = list()
  ), class = "multistar"))

  expect_equal(class(nms), "multistar")


  expect_equal(class(nms), "multistar")

  expect_equal(
    c(names(nms$fact), names(nms$dimension)),
    c(
      "mrs_age",
      "when",
      "when_available",
      "where",
      "who",
      "when_common"
    )
  )

  expect_equal(class(ft), c("tbl_df", "tbl", "data.frame"))

  expect_equal(
    names(ft),
    c(
      "age_range",
      "wide_age_range",
      "division",
      "state",
      "city",
      "division_name",
      "region",
      "region_name",
      "state_name",
      "county",
      "data_availability_date",
      "data_availability_week",
      "data_availability_year",
      "when_happened_date",
      "when_happened_week",
      "when_happened_year",
      "n_deaths",
      "nrow_agg"
    )
  )

})
