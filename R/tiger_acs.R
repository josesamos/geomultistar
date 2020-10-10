#' `tiger_acs` S3 class
#'
#' Internal low-level constructor that creates new objects with the correct
#' structure.
#'
#' @importFrom magrittr %>%
#' @name %>%
#'
#' @param filepath A string, path to gbd file.
#' @param code A string.
#'
#' @return A `tiger_acs` object.
#'
#' @keywords internal
new_tiger_acs <- function(filepath = NULL, code = NULL) {
  # Use `st_layers' to list all layer names and their type in a data source.
  # Set the `layer' argument in `st_read' to read a particular layer.
  layers <- sf::st_layers(dsn = filepath)
  layer_names <- sort(layers$name)

  metadata <-
    sf::st_read(dsn = filepath,
                layer = layer_names[2],
                quiet = TRUE) %>%
    tibble::as_tibble()

  metadata$inf_code <- ""
  ##############
  for (i in seq_along(metadata[[1]])) {
    short <- strsplit(metadata$Short_Name[i], "")[[1]]
    metadata$inf_code[i] <- paste(short[1:3], collapse = "")
  }
  metadata <- metadata %>%
    dplyr::filter(inf_code == code)
  ##############

  # metadata$inf_code <- ""
  metadata$group_code <- ""
  metadata$subgroup_code <- ""
  metadata$type_code <- ""
  metadata$spec_code <- ""

  metadata$type <- ""
  metadata$group <- ""
  metadata$subgroup <- ""

  metadata$genre <- ""
  metadata$age <- ""
  metadata$human_group <- ""
  metadata$human_group_spec <- ""
  metadata$total <- ""
  metadata$total_population <- ""

  metadata$ancestry <- ""
  metadata$ancestry_spec <- ""

  metadata$place_of_birth <- ""
  metadata$place_of_birth_spec <- ""
  metadata$u_s_citizen <- ""
  metadata$u_s_citizen <- ""
  metadata$condition <- ""

  metadata$rest <- ""
  acs <-
    list(
      layers = layer_names,
      metadata = metadata
    )

  structure(acs,
            class = "tiger_acs")
}

#' `tiger_acs` S3 class
#'
#' A `tiger_acs` object is created from a given
#'
#' @param filepath A string, path to gbd file.
#' @param code A string.
#'
#' @return A `tiger_acs` object.
#'
#'
#' @export
tiger_acs <-
  function(filepath = NULL, code = NULL) {
    new_tiger_acs(filepath, code)
  }



# transform_metadata ------------------------------------------------------

#' Transform metadata
#'
#' @param ta A string.
#'
#' @return A `tiger_acs` object.
#'
#' @keywords internal
transform_metadata <- function(ta) {
  UseMethod("transform_metadata")
}


#' @rdname transform_metadata
#' @export
#' @keywords internal
transform_metadata.tiger_acs <- function(ta) {
  for (i in seq_along(ta$metadata[[1]])) {
    ta$metadata[i,] <- transform_code(ta$metadata[i,])

    values <- strsplit(ta$metadata$Full_Name[i], ":")[[1]]
    values <- stringr::str_trim(values, side = "both")

    ta$metadata[i,] <- transform_values(ta$metadata[i,], values)
  }
  ta
}


#' Transform code
#'
#' @param mdr A `tibble` row.
#'
#' @return A `tibble` row.
#'
#' @keywords internal
transform_code <- function(mdr) {
  i <- 1
  short <- strsplit(mdr$Short_Name[i], "")[[1]]
  mdr$inf_code[i] <- paste(short[1:3], collapse = "")
  mdr$group_code[i] <- paste(short[4:6], collapse = "")
  pos <- which(short %in% c("e", "m"))
  if (pos > 7) {
    mdr$subgroup_code[i] <- paste(short[7:(pos-1)], collapse = "")
  }
  mdr$type_code <- short[pos]
  mdr$spec_code[i] <- paste(short[(pos+1):length(short)], collapse = "")
  if (short[pos] == "e") {
    mdr$Full_Name[i] <- stringr::str_replace(mdr$Full_Name[i], " -- \\(Estimate\\)", "")
    mdr$type[i] <- "Estimate"
  } else if (short[pos] == "m") {
    mdr$Full_Name[i] <- stringr::str_replace(mdr$Full_Name[i], " -- \\(Margin of Error\\)", "")
    mdr$type[i] <- "Margin of Error"
  }
  mdr
}


#' Transform code
#'
#' @param mdr A `tibble` row.
#' @param values A vector of values
#'
#' @return A `tibble` row.
#'
#' @keywords internal
transform_values <- function(mdr, values) {
  val <- tolower(values)
  val <- snakecase::to_snake_case(val, sep_out = "_")

  if (mdr$subgroup_code == "" |
      # Puerto Rico
      mdr$subgroup_code == "PR") {
    mdr$group <- values[1]
  } else {
    # group: remove the content of the parentheses
    mdr$group <- gsub("\\s*\\([^\\)]+\\)", "", values[1])
    # subgroup: content in parentheses
    mdr$subgroup <-
      regmatches(values[1], gregexpr("(?<=\\().*?(?=\\))", values[1], perl = T))[[1]]
  }

  for (j in 2:length(val)) {
    mdr <- read_b01(mdr, val[j], values[j])
  }
  mdr
}




read_b01 <- function(mdr, val, value) {
  if (val %in% c("male", "female")) {
    mdr$genre <- value
  } else if (val %in% c("total")) {
    mdr$total <- value
  } else if (val %in% c("total_population") |
             grepl("total_population_in", val, fixed = TRUE)) {
    mdr$total_population <- value
  } else if (grepl("black_or_african", val, fixed = TRUE) |
             grepl("hispanic_or_latino", val, fixed = TRUE) |
             grepl("american_indian", val, fixed = TRUE) |
             grepl("asian_alone", val, fixed = TRUE) |
             grepl("native_hawaiian", val, fixed = TRUE) |
             grepl("other_race_alone", val, fixed = TRUE) |
             grepl("two_or_more_races", val, fixed = TRUE) |
             grepl("white_alone", val, fixed = TRUE) |
             grepl("groups_tallied", val, fixed = TRUE) |
             grepl("alaska_native", val, fixed = TRUE) |
             grepl("central_american", val, fixed = TRUE) |
             grepl("south_american", val, fixed = TRUE)) {
    mdr$human_group <- add_value(mdr$human_group, value)
  } else if (mdr$inf_code %in% c("B02", "B03")) {
    mdr$human_group_spec <- add_value(mdr$human_group_spec, value)
  } else if (grepl("single_ancestry", val, fixed = TRUE) |
             grepl("multiple_ancestr", val, fixed = TRUE) |
             grepl("ancestry_specified", val, fixed = TRUE) |
             grepl("ancestry_not", val, fixed = TRUE) |
             grepl("ancestry_un", val, fixed = TRUE)) {
    mdr$ancestry <- add_value(mdr$ancestry, value)
  } else if (mdr$inf_code %in% c("B04")) {
    mdr$ancestry_spec <- add_value(mdr$ancestry_spec, value)
  } else if (grepl("u_s_citizen", val, fixed = TRUE) |
             grepl("not_a_u_s_citizen", val, fixed = TRUE) |
             grepl("naturalized_u_s", val, fixed = TRUE) |
             grepl("naturalized_citizens", val, fixed = TRUE)) {
    mdr$u_s_citizen <- add_value(mdr$u_s_citizen, value)
  } else if (grepl("born_in", val, fixed = TRUE) |
             grepl("born_outside", val, fixed = TRUE) |
             grepl("foreign_born", val, fixed = TRUE) |
             grepl("native", val, fixed = TRUE)) {
    mdr$place_of_birth <- add_value(mdr$place_of_birth, value)
  } else if ((grepl("entered_", val, fixed = TRUE) &
              (
                grepl("_to_", val, fixed = TRUE) |
                grepl("_before", val, fixed = TRUE) |
                grepl("_or_later", val, fixed = TRUE)
              )) |
             grepl("living_with_", val, fixed = TRUE) |
             grepl("under_1_00", val, fixed = TRUE) |
             grepl("1_00_to_1_99", val, fixed = TRUE) |
             grepl("2_00_and_over", val, fixed = TRUE) |
             (grepl("naturalized_", val, fixed = TRUE) &
              (
                grepl("_to_", val, fixed = TRUE) |
                grepl("before_", val, fixed = TRUE)
              )) | grepl("own_children_", val, fixed = TRUE)) {
    mdr$condition <- add_value(mdr$condition, value)
  }  else if (((
    grepl("under_", val, fixed = TRUE) |
    grepl("_to_", val, fixed = TRUE) |
    grepl("_and_", val, fixed = TRUE)
  ) &
  grepl("_years", val, fixed = TRUE)) |
  val %in% c("20_years", "21_years")) {
    mdr$age <- add_value(mdr$age, value)
  } else if (mdr$inf_code %in% c("B05")) {
    mdr$place_of_birth_spec <- add_value(mdr$place_of_birth_spec, value)
  } else {
    mdr$rest <- add_value(mdr$rest, val)
  }
  mdr
}


add_value <- function(field, value, sep = ": ") {
  if (field == "") {
    field <- value
  } else {
    field <- paste(field, value, sep = sep)
  }
  field
}


