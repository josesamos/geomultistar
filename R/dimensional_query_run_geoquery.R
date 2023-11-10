
#' Get a geographic vector from a query
#'
#' After defining a query and geographic dimensions, run the query and select
#' the geographic data associated with it to get a geographic data layer as the
#' result.
#'
#' In the case of having several fact tables, as an option, we can indicate if
#' we do not want to unify the facts in the case of having the same grain.
#'
#' If the result only has one fact table, it is not necessary to provide its
#' name. Nor is it necessary to indicate the name of the geographic dimension if
#' there is only one available.
#'
#' If no attribute is specified, the geographic attribute of the result with
#' finer granularity is selected.
#'
#' In geographic layers, geographic objects are not repeated. The tables are
#' wide: for each object the rest of the attributes are defined as columns. By
#' means of the parameter `wider` we can indicate that we want a result of this
#' type.
#'
#' @param dq A `dimensional_query` object.
#' @param unify_by_grain A boolean, unify facts with the same grain.
#' @param fact A string, name of the fact.
#' @param dimension A string, name of the geographic dimension.
#' @param attribute A string, name of the geographic attribute to consider.
#' @param wider A boolean, avoid repeating geographic data.
#'
#' @return A `sf` object.
#'
#' @family geo functions
#'
#' @examples
#'
#' gms <- geomultistar(ms = ms_mrs, geodimension = "where") |>
#'   define_geoattribute(
#'     attribute = "city",
#'     from_layer = usa_cities,
#'     by = c("city" = "city", "state" = "state")
#'   )  |>
#'   define_geoattribute(
#'     attribute = "state",
#'     from_layer = usa_states,
#'     by = c("state" = "state")
#'   ) |>
#'   define_geoattribute(attribute = "region",
#'                       from_attribute = "state") |>
#'   define_geoattribute(attribute = "all_where",
#'                       from_layer = usa_nation)
#'
#' gdq <- dimensional_query(gms) |>
#'   select_dimension(name = "where",
#'                    attributes = c("state", "city")) |>
#'   select_dimension(name = "when",
#'                    attributes = c("when_happened_year", "when_happened_week")) |>
#'   select_fact(
#'     name = "mrs_age",
#'     measures = c("n_deaths")
#'   ) |>
#'   select_fact(name = "mrs_cause",
#'               measures = c("pneumonia_and_influenza_deaths", "other_deaths")) |>
#'   filter_dimension(name = "when", when_happened_week <= "03") |>
#'   filter_dimension(name = "where", state == "MA")
#'
#' sf <- gdq |>
#'   run_geoquery()
#'
#' sfw <- gdq |>
#'   run_geoquery(wider = TRUE)
#'
#' @export
run_geoquery <-
  function(dq,
           unify_by_grain = TRUE,
           fact = NULL,
           dimension = NULL,
           attribute = NULL,
           wider = FALSE) {
    UseMethod("run_geoquery")
  }

#' @rdname run_geoquery
#' @export
run_geoquery.dimensional_query <-
  function(dq,
           unify_by_grain = TRUE,
           fact = NULL,
           dimension = NULL,
           attribute = NULL,
           wider = FALSE) {
    dq <- add_geodimension_additional_attributes(dq)
    dq$output <- run_query(dq, unify_by_grain)
    ft <- multistar_as_flat_table(dq$output, fact)
    columns <- names(ft)

    if (is.null(dimension)) {
      dimension <- names(dq$input$geodimension)[1]
    }
    if (is.null(attribute)) {
      attribute <- default_attribute(dq, dimension)
    }
    is_geographic_attribute <- attribute %in% names(dq$input$geodimension[[dimension]])
    stopifnot(is_geographic_attribute)

    geodim <- dq$input$geodimension[[dimension]][[attribute]]
    if (attribute == sprintf("all_%s", dimension)) {
      # all, geographic dimension not selected in query
      ft <- tibble::add_column(ft, !!attribute := attribute, .before = 1)
      # fk to join to the layer
      pk <- attribute
    } else {
      names_geodim <- names(geodim)
      pk <- names_geodim[-length(names_geodim)]
    }

    if (wider) {
      measures <- get_selected_measure_names(dq, ft)
      l <- widen_flat_table(ft, pk, measures)
      ft <- l$sf
    }
    ft <- dplyr::left_join(ft, geodim, by = pk) |>
      sf::st_as_sf()

    if (wider) {
      l$sf <- ft
      l
    } else {
      ft <- ft |>
        dplyr::select(tidyselect::all_of(columns)) |>
        dplyr::group_by_at(columns) |>
        dplyr::summarize(.groups = "drop")
    }
  }

#' Default attribute
#'
#' Get the attribute with the most instances, if there is more than one.
#'
#' @param dq A `dimensional_query` object.
#' @param dimension A string, name of the geographic dimension.
#'
#' @return A string, name of the attribute.
#'
#' @keywords internal
default_attribute <- function(dq, dimension) {
  attribute  <- sprintf("all_%s", dimension)
  if (!is.null(dq$output$dimension[[dimension]])) {
    candidates <- names(dq$output$dimension[[dimension]])[-1]
    n_instances <- 0
    for (candidate in candidates) {
      if (!is.null(dq$input$geodimension[[dimension]][[candidate]])) {
        ni <-
          attr(dq$input$geodimension[[dimension]][[candidate]], "n_instances")
        if (ni > n_instances) {
          attribute <- candidate
          n_instances <- ni
        }
      }
    }
  }
  attribute
}



#' Add geodimension additional attributes
#'
#' Adds the geodimension attributes not explicitly selected in the query but
#' included in the definition.
#'
#' @param dq A `dimensional_query` object.
#'
#' @return A `dimensional_query` object.
#'
#' @keywords internal
add_geodimension_additional_attributes <- function(dq) {
  names_geodimension <- names(dq$input$geodimension)
  for (dimension in names(dq$dimension)) {
    if (dimension %in% names_geodimension) {
      sel_attributes <- dq$dimension[[dimension]][-1] # no pk
      for (attribute in sel_attributes) {
        names_attribute <- names(dq$input$geodimension[[dimension]][[attribute]])
        if (length(names_attribute) > 2) {
          names_attribute <- names_attribute[-length(names_attribute)]
          dq$dimension[[dimension]] <- unique(c(dq$dimension[[dimension]], names_attribute))
        }
      }
    }
  }
  dq
}


#' widen_flat_table
#'
#' Makes the pk fields the primary key of the table. The rest of the fields that
#' prevent them from being so, are extracted to another table. The original
#' table is widened by combining the extracted fields with the measures.
#'
#' @param ft A `tibble` object.
#' @param pk A vector of names.
#' @param measures A vector of names.
#'
#' @return A list of `tibble` objects.
#' @keywords internal
widen_flat_table <- function(ft, pk, measures) {
  names_ft <- names(ft)
  stopifnot(pk %in% names_ft)
  stopifnot(measures %in% names_ft)
  pk <- unique(pk)
  measures <- unique(measures)
  rest <- names_ft[!(names_ft %in% c(pk, measures))]

  ft_pk <- unique(ft[, pk])
  rest_out <- NULL
  rest_in <- NULL
  for (at in rest) {
    ft_pk_at <- unique(ft[, c(pk, at)])
    if (nrow(ft_pk) < nrow(ft_pk_at)) {
      rest_out <- c(rest_out, at)
    } else {
      rest_in <- c(rest_in, at)
    }
  }
  pk <- c(pk, rest_in)

  length_rest_out <- length(rest_out)
  if (length_rest_out > 0) {
    ft_out <- unique(ft[, rest_out])
    if (length_rest_out > 1) {
      n_row <- nrow(ft_out)
      format <- sprintf("V%%0%dd", max(nchar(sprintf("%s", n_row))))
      id <- sprintf(format, 1:n_row)
      ft_out <- tibble::add_column(ft_out, id_variable = id, .before = 1)
    } else if (length_rest_out == 1) {
      ft_out <- tibble::add_column(ft_out, id_variable = as.character(ft_out[, rest_out][[1]]), .before = 1)
    }

    ft <- dplyr::left_join(ft, ft_out, by = rest_out)
    ft <- dplyr::select(ft, !tidyselect::all_of(rest_out))
    names_ft <- names(ft)
    ft <- tidyr::pivot_wider(ft, names_from = names_ft[length(names_ft)], values_from = tidyselect::all_of(measures))
    ft <- tibble::add_column(ft, fid = 1:nrow(ft), .before = 1)

    out <- tibble::add_column(ft_out, measure = measures[1], .after = 1)
    for (m in measures[-1]) {
      out <- tibble::add_row(out, tibble::add_column(ft_out, measure = m, .after = 1))
    }
    measure <- names(out)[2]
    out$id_variable <- paste(out[, measure][[1]], out$id_variable, sep = "_")

    list(sf = ft, variables = out)
  } else {
    list(sf = ft)
  }
}

#' get_selected_measure_names
#'
#' Get the names of the measures selected in the query.
#'
#' @param dq A `dimensional_query` object.
#' @param ft A `tibble` object.
#'
#' @return A vector of names.
#' @keywords internal
get_selected_measure_names <- function(dq, ft) {
  num_measures <- 0
  for (name in names(dq$fact)) {
    num_measures <- num_measures + length(names(dq$fact[[name]]))
  }
  names_ft <- names(ft)
  names_ft[(length(names_ft) - num_measures + 1):length(names_ft)]
}


