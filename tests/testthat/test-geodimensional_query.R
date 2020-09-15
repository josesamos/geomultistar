context("test geodimensional_query")

test_that("geodimensional_query works", {
  dq <- geodimensional_query(starschemar::ms_mrs_test)

  expect_equal(names(dq), c("fact", "dimension", "key", "input", "output"))
  expect_equal(names(dq$input), c("fact", "dimension"))
})
