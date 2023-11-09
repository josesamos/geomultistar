
#' Reference a dimension
#'
#' Given a dimension, transform the fact table so that the attributes of the
#' dimension indicated as a parameter, which are in the fact table, are replaced
#' by the other attributes of the dimension.
#'
#' It is used to replace a set of attributes in the fact table with the
#' generated key of the dimension.
#'
#' If necessary, it is also used for the inverse operation: replace the
#' generated key with the rest of attributes (dereference a dimension).
#'
#' @param ft A `fact_table` object.
#' @param dimension A `dimension_table` object.
#' @param attributes A vector of attribute names, attributes used to reference the dimension.
#' @param conversion A boolean, indicates whether the attributes need to be
#'   transformed.
#'
#' @return A `fact_table` object.
#'
#' @keywords internal
reference_dimension <-
  function(ft, dimension, attributes, conversion = TRUE) {
    if (conversion) {
      dimension[, -1] <- prepare_join(dimension[, -1]) # except key
    }
    # union with dimension
    ft <- dplyr::inner_join(ft, dimension, by = attributes)
    # remove attributes from dimension
    ft <- ft[,-which(names(ft) %in% attributes)]
    # place rest of them on the left
    for (i in 1:(length(names(dimension)) - length(attributes))) {
      ft <- dplyr::relocate(tibble::as_tibble(ft), tidyr::last_col())
    }
    # restore the object class
    class(ft) <-  unique(c("fact_table", class(ft)))
    ft
  }



#' Dereference a dimension
#'
#' Given a dimension, transform the fact table so that the primary key of the
#' dimension (which is a foreign key in the fact table) is replaced by the other
#' attributes of the dimension.
#'
#' @param ft A `fact_table` object.
#' @param dimension A `dimension_table` object.
#' @param conversion A boolean, indicates whether the attributes need to be
#'   transformed.
#'
#' @return A `fact_table` object.
#'
#' @keywords internal
dereference_dimension <-
  function(ft, dimension, conversion = TRUE) {
    reference_dimension(ft, dimension, names(dimension)[1], conversion)
  }


# prepare_join ------------------------------------------------------------

#' Transform a `tibble` to join
#'
#' Transform all fields in a `tibble` to character type and replace the `NA`
#' with a specific value.
#'
#' @param tb A `tibble`.
#'
#' @return A `tibble`.
#' @keywords internal
prepare_join <- function(tb) {
  n_row <- nrow(tb)
  # all attributes of type character
  col <- colnames(tb)
  tb <- data.frame(lapply(tb, as.character), stringsAsFactors = FALSE)
  colnames(tb) <- col

  # replace NA with unknown (for join)
  tb <- apply(tb[, , drop = FALSE], 2, function(x)
    tidyr::replace_na(x, "___UNKNOWN___"))
  if (n_row == 1) {
    tibble::as_tibble_row(tb)
  } else {
    tibble::as_tibble(tb)
  }
}



#' Group the records in the table
#'
#' Group the records in the table using the aggregation functions for the
#' measurements.
#'
#' @param ft A `fact_table` object.
#'
#' @return A `fact_table` object.
#'
#' @keywords internal
group_table <- function(ft) {
  at <- attributes(ft)
  measures <- attr(ft, "measures")
  dim_keys <- setdiff(names(ft), measures)
  ft_group <- dplyr::group_by(as.data.frame(ft), dplyr::across(dplyr::all_of(dim_keys)))
  agg <- list()
  for (i in seq_along(measures)) {
    if (at$agg_functions[i] == "MAX") {
      df <-
        dplyr::summarize_at(ft_group, dplyr::vars(at$measures[i]), max, na.rm = TRUE)
    } else if (at$agg_functions[i] == "MIN") {
      df <-
        dplyr::summarize_at(ft_group, dplyr::vars(at$measures[i]), min, na.rm = TRUE)
    } else {
      df <-
        dplyr::summarize_at(ft_group, dplyr::vars(at$measures[i]), sum, na.rm = TRUE)
    }
    agg <- c(agg, list(df))
  }
  ft <- purrr::reduce(agg, dplyr::inner_join, by = dim_keys)

  new_fact_table(
    tibble::as_tibble(ft),
    name = at$name,
    measures = at$measures,
    agg_functions = at$agg_functions,
    nrow_agg = at$nrow_agg
  )
}


#' `fact_table` S3 class
#'
#' Internal low-level constructor that creates new objects with the correct
#' structure.
#'
#' @param ft A `tibble`, contains the fact table.
#' @param name A string, name of the fact.
#' @param measures A vector of measurement names.
#' @param agg_functions A vector of aggregation function names.
#' @param nrow_agg A string, measurement name for the number of rows aggregated.
#'
#' @return A `fact_table` object.
#'
#' @keywords internal
new_fact_table <-
  function(ft = tibble::tibble(),
           name = NULL,
           measures = NULL,
           agg_functions = NULL,
           nrow_agg = NULL) {
    # Check the type of the base object
    stopifnot(tibble::is_tibble(ft))
    stopifnot(!is.null(name))

    fk <- c()
    for (n in names(ft)) {
      if (!(n %in% measures)) {
        fk <- c(fk, n)
      }
    }

    structure(
      ft,
      class = unique(c("fact_table", class(ft))),
      name = name,
      foreign_keys = fk,
      measures = measures,
      agg_functions = agg_functions,
      nrow_agg = nrow_agg
    )
  }

