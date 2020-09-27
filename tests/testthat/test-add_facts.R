context("test add_facts")

test_that("add_facts works", {
  ms <- multistar()
  ms <- add_facts(
    ms,
    fact_name = "mrs_age",
    fact_table = mrs_fact_age,
    measures = "n_deaths",
    nrow_agg = "count"
  )
  expect_equal(
    attributes(ms$fact$mrs_age)[-c(1, 2)],
    list(
      class = c("tbl_df", "tbl", "data.frame", "fact_table"),
      name = "mrs_age",
      measures = c("n_deaths", "count"),
      agg_functions = c("SUM",
                        "SUM"),
      nrow_agg = "count"
    )
  )

  ms <- add_facts(
    ms,
    fact_name = "mrs_cause",
    fact_table = mrs_fact_cause,
    measures = c("pneumonia_and_influenza_deaths", "other_deaths"),
    nrow_agg = "nrow_agg"
  )
  expect_equal(
    attributes(ms$fact$mrs_cause)[-c(1, 2)],
    list(
      class = c("tbl_df", "tbl", "data.frame", "fact_table"),
      name = "mrs_cause",
      measures = c("pneumonia_and_influenza_deaths",
                   "other_deaths", "nrow_agg"),
      agg_functions = c("SUM", "SUM",
                        "SUM"),
      nrow_agg = "nrow_agg"
    )
  )
})
