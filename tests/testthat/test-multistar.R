context("test multistar")

test_that("multistar works", {
  ms <- multistar()

  expect_equal(ms, structure(list(
    fact = list(), dimension = list()
  ), class = "multistar"))
})
