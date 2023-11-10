context("test relate_dimension")

test_that("relate_dimension works", {
  ms <- multistar()
  ms <- add_facts(
    ms,
    fact_name = "mrs_age",
    fact_table = mrs_fact_age,
    measures = "n_deaths",
    nrow_agg = "count"
  )
  ms <- add_facts(
    ms,
    fact_name = "mrs_cause",
    fact_table = mrs_fact_cause,
    measures = c("pneumonia_and_influenza_deaths", "other_deaths"),
    nrow_agg = "nrow_agg"
  )
  ms <- add_dimension(
    ms,
    dimension_name = "where",
    dimension_table = mrs_where,
    dimension_key = "where_pk",
    fact_name = "mrs_age",
    fact_key = "where_fk"
  )

  ms <- relate_dimension(
    ms,
    dimension_name = "where",
    fact_name = "mrs_cause",
    fact_key = "where_fk"
  )

  expect_equal(
    attributes(ms$fact$mrs_cause),
    list(
      names = c(
        "where_key",
        "when_fk",
        "pneumonia_and_influenza_deaths",
        "other_deaths",
        "count",
        "nrow_agg"
      ),
      row.names = 1:1278,
      class = c("tbl_df",
                "tbl", "data.frame", "fact_table"),
      name = "mrs_cause",
      measures = c("pneumonia_and_influenza_deaths",
                   "other_deaths", "nrow_agg"),
      agg_functions = c("SUM", "SUM",
                        "SUM"),
      nrow_agg = "nrow_agg",
      foreign_keys = "where_key"
    )
  )
})
