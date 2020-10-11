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
  print(sort(unique(metadata$inf_code)))
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

  metadata$sex <- ""
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

  metadata$situation <- ""

  metadata$income <- ""
  metadata$money <- ""
  metadata$marital_status <- ""
  metadata$languages <- ""
  metadata$english <- ""
  metadata$studies <- ""

  metadata$transportation_to_work <- ""
  metadata$activity <- ""
  metadata$activity_spec <- ""
  metadata$workplace <- ""
  metadata$residence <- ""
  metadata$vehicles <- ""

  metadata$disability <- ""
  metadata$insurance_coverage <- ""

  metadata$householder <- ""

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
    # subgroup: content in parentheses
    subgroup <- regmatches(values[1], gregexpr("(?<=\\().*?(?=\\))", values[1], perl = T))[[1]]
    mdr$subgroup <- subgroup[length(subgroup)]
    # group: remove the content of the parentheses
    # mdr$group <- gsub("\\s*\\([^\\)]+\\)", "", values[1])
    mdr$group <- stringr::str_replace(values[1], sprintf("\\(%s\\)", mdr$subgroup), "")
  }

  for (j in 2:length(val)) {
    mdr <- read_b01(mdr, val[j], values[j])
  }
  mdr
}




read_b01 <- function(mdr, val, value) {
  if (val %in% c("male", "female")) {
    mdr$sex <- value
  } else if (grepl("married_couple", val, fixed = TRUE) |
              grepl("male_householder", val, fixed = TRUE)) {
    mdr$householder <- add_value(mdr$householder, value)
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
  } else if (mdr$inf_code %in% c("B11", "B22")) {
    mdr$condition <- add_value(mdr$condition, value)
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
  } else if ((
    grepl("born_in", val, fixed = TRUE) |
    grepl("born_outside", val, fixed = TRUE) |
    grepl("foreign_born", val, fixed = TRUE) |
    (grepl("native", val, fixed = TRUE) & !grepl("languages", val, fixed = TRUE))
  ) & (!mdr$inf_code %in% c("B15"))) {
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
              )) |
             grepl("own_children_", val, fixed = TRUE) |
             grepl("_years_and_over_in_", val, fixed = TRUE) |
             grepl("_poverty", val, fixed = TRUE) |
             grepl("some_other_race_population_in_puerto_rico", val, fixed = TRUE) |
             grepl("population_1_year_and_", val, fixed = TRUE) |
             (mdr$inf_code %in% c("B13") &
              substr(val, 1, 6) == "women_")) {
    mdr$condition <- add_value(mdr$condition, value)
  }  else if (((
    grepl("under_", val, fixed = TRUE) |
    grepl("_to_", val, fixed = TRUE) |
    grepl("_and_", val, fixed = TRUE)
  ) &
  grepl("_years", val, fixed = TRUE) &
  (!grepl("population_", val, fixed = TRUE)) &
  (!grepl("total_bachelor", val, fixed = TRUE)) &
  (!grepl("related_children", val, fixed = TRUE)) &
  (!grepl("civilian_veterans", val, fixed = TRUE)) &
  (!grepl("females_20", val, fixed = TRUE)) &
  (!grepl("no_children_under", val, fixed = TRUE)) &
  (!mdr$inf_code %in% c("B10"))
  ) |
  val %in% c("20_years", "21_years", "5_years", "15_years")) {
    mdr$age <- add_value(mdr$age, value)
  } else if (mdr$inf_code %in% c("B17", "B23")) {
    mdr$condition <- add_value(mdr$condition, value)
  } else if (mdr$inf_code %in% c("B05")) {
    mdr$place_of_birth_spec <- add_value(mdr$place_of_birth_spec, value)
  } else if (grepl("income", val, fixed = TRUE)) {
    mdr$income <- add_value(mdr$income, value)
  } else if (grepl("_000_or_more", val, fixed = TRUE) |
             grepl("_000_to_", val, fixed = TRUE) |
             grepl("1_to_9_999_", val, fixed = TRUE) |
             grepl("_499", val, fixed = TRUE) |
             grepl("_999", val, fixed = TRUE) |
             grepl("less_than_10_000", val, fixed = TRUE)) {
    mdr$money <- add_value(mdr$money, value)
  } else if ((!mdr$inf_code %in% c("B09")) &
             (
               grepl("divorced", val, fixed = TRUE) |
               grepl("married", val, fixed = TRUE) |
               grepl("separated", val, fixed = TRUE) |
               grepl("widowed", val, fixed = TRUE)
             )) {
    mdr$marital_status <- add_value(mdr$marital_status, value)
  } else if (substr(val, 1, 11) == "speak_only_" |
             substr(val, 1, 12) == "speak_other_" |
             substr(val, 1, 13) == "speak_spanish") {
    mdr$languages <- add_value(mdr$languages, value)
  } else if (substr(val, 1, 14) == "speak_english_") {
    mdr$english <- add_value(mdr$english, value)
  } else if (grepl("_degree", val, fixed = TRUE) |
             grepl("_graduate", val, fixed = TRUE) |
             grepl("graduate_", val, fixed = TRUE) |
             (mdr$inf_code %in% c("B14") &
              (
                grepl("enrolled_in_", val, fixed = TRUE) |
                grepl("_school", val, fixed = TRUE)
              ))) {
    mdr$studies <- add_value(mdr$studies, value)
  } else if (mdr$inf_code %in% c("B07")) {
    mdr$situation <- add_value(mdr$situation, value)
  } else if ((
    grepl("agriculture", val, fixed = TRUE) |
    grepl("construction", val, fixed = TRUE) |
    grepl("manufacturing", val, fixed = TRUE) |
    grepl("_trade", val, fixed = TRUE) |
    grepl("transportation_and", val, fixed = TRUE) |
    grepl("information", val, fixed = TRUE) |
    grepl("finance_and", val, fixed = TRUE) |
    grepl("professional", val, fixed = TRUE) |
    grepl("educational", val, fixed = TRUE) |
    grepl("arts_", val, fixed = TRUE) |
    grepl("other_services", val, fixed = TRUE) |
    grepl("_occupations", val, fixed = TRUE) |
    grepl("public_administration", val, fixed = TRUE) |
    grepl("_workers", val, fixed = TRUE) |
    grepl("armed_forces", val, fixed = TRUE)
  ) &
  (!mdr$inf_code %in% c("B15", "B24"))) {
    mdr$activity <- add_value(mdr$activity, value)
  } else if (grepl("worked_in_", val, fixed = TRUE) |
             grepl("worked_outside_", val, fixed = TRUE)) {
    mdr$workplace <- add_value(mdr$workplace, value)
  } else if (grepl("living_", val, fixed = TRUE)) {
    mdr$residence <- add_value(mdr$residence, value)
  } else if (grepl("_vehicle_available", val, fixed = TRUE) |
             grepl("_vehicles_available", val, fixed = TRUE) |
             grepl("_vehicles_available", val, fixed = TRUE)) {
    mdr$vehicles <- add_value(mdr$vehicles, value)
  } else if (grepl("_difficulty", val, fixed = TRUE) |
             grepl("_disability", val, fixed = TRUE)) {
    mdr$disability <- add_value(mdr$disability, value)
  } else if (grepl("_insurance_coverage", val, fixed = TRUE) |
             grepl("_health_coverage", val, fixed = TRUE)) {
    mdr$insurance_coverage <- add_value(mdr$insurance_coverage, value)
  } else if (mdr$inf_code %in% c("B08")) {
    mdr$transportation_to_work <- add_value(mdr$transportation_to_work, value)
  } else if (mdr$inf_code %in% c("B24")) {
    if (mdr$activity == "") {
      mdr$activity <- value
    } else {
      mdr$activity_spec <- add_value(mdr$activity_spec, value)
    }
  } else if (mdr$inf_code %in% c("B09", "B10", "B12", "B13", "B14", "B18", "B19", "B20", "B21")) {
    mdr$condition <- add_value(mdr$condition, value)
  } else if (mdr$inf_code %in% c("B15", "B16") &
             (substr(val, 1, 11) == "population_" |
              substr(val, 1, 14) == "in_labor_force" |
              substr(val, 1, 18) == "not_in_labor_force")) {
    mdr$condition <- add_value(mdr$condition, value)
  } else if (mdr$inf_code %in% c("B15")) {
    mdr$studies <- add_value(mdr$studies, value)
  } else if (mdr$inf_code %in% c("B16")) {
    mdr$languages <- add_value(mdr$languages, value)
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


