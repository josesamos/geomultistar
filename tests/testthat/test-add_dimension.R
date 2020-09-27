context("test add_dimension")

test_that("add_dimension works", {
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

  expect_equal(
    attributes(ms$dimension$where),
    list(
      names = c(
        "where_key",
        "region",
        "region_name",
        "division",
        "division_name",
        "state",
        "state_name",
        "county",
        "city"
      ),
      row.names = 1:121,
      class = c("tbl_df", "tbl", "data.frame", "dimension_table"),
      name = "where",
      type = "general"
    )
  )

  expect_equal(
    attributes(ms$fact$mrs_age),
    list(
      names = c("where_key", "when_fk", "who_fk", "n_deaths",
                "count"),
      row.names = 1:5855,
      class = c("tbl_df", "tbl", "data.frame",
                "fact_table"),
      name = "mrs_age",
      measures = c("n_deaths", "count"),
      agg_functions = c("SUM", "SUM"),
      nrow_agg = "count",
      foreign_keys = "where_key"
    )
  )

  ms <- add_dimension(
    ms,
    dimension_name = "when",
    dimension_table = mrs_when,
    dimension_key = "when_pk",
    fact_name = "mrs_age",
    fact_key = "when_fk",
    key_as_data = TRUE
  )
  expect_equal(
    attributes(ms$dimension$when),
    list(
      names = c("when_key", "when_pk", "date", "week", "year"),
      row.names = 1:85,
      class = c("tbl_df", "tbl", "data.frame",
                "dimension_table"),
      name = "when",
      type = "general"
    )
  )

  expect_equal(
    attributes(ms$fact$mrs_age),
    list(
      names = c("where_key", "when_key", "who_fk", "n_deaths",
                "count"),
      row.names = 1:5855,
      class = c("tbl_df", "tbl", "data.frame",
                "fact_table"),
      name = "mrs_age",
      measures = c("n_deaths", "count"),
      agg_functions = c("SUM", "SUM"),
      nrow_agg = "count",
      foreign_keys = c("where_key",
                       "when_key")
    )
  )
})
