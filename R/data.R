
#' USA Cities, 2014
#'
#' From the original dataset, some fields have been selected and renamed, and
#' only includes the Mortality Reporting System cities.
#'
#' @examples
#'
#' # Read by:
#' #
#' # filepath <- "data/citiesx010g/citiesx010g.shp"
#' # usa_cities <- st_read(filepath)  |>
#' #   dplyr::select(
#' #     gnis_id = GNIS_ID,
#' #     ansi_code = ANSICODE,
#' #     city = NAME,
#' #     state = STATE,
#' #     county = COUNTY,
#' #     latitude = LATITUDE,
#' #     longitude = LONGITUDE,
#' #     elev_m = ELEV_IN_M
#' #   )
#'
#' @format A `sf`.
#' @source
#'   \url{https://earthworks.stanford.edu/catalog/stanford-bx729wr3020}
"usa_cities"


#' USA Counties, 2018
#'
#' From the original dataset, some fields have been selected and renamed, and
#' only includes the Mortality Reporting System counties.
#'
#' Some counties appear with the same repeated name within the same state, they
#' are the following: Baltimore, MD; Richmond, VA; St. Louis, MO. Since they are
#' accessed by name (county and state), those of the same name within the state
#' have been grouped together.
#'
#' @examples
#'
#' # Read by:
#' #
#' # filepath <- "data/cb_2018_us_county_20m/cb_2018_us_county_20m.shp"
#' # usa_counties <- st_read(filepath)  |>
#' #   dplyr::select(
#' #     geo_id = GEOID,
#' #     state_fp = STATEFP,
#' #     county_fp = COUNTYFP,
#' #     county = NAME
#' #   )
#' # states <- sf::st_drop_geometry(usa_states[, c("geo_id", "state")])
#' # usa_counties <- usa_counties |>
#' #  dplyr::left_join(states, by = c("state_fp" = "geo_id"))
#'
#' @format A `sf`.
#' @source
#'   \url{https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_county_20m.zip}
"usa_counties"


#' USA States, 2018
#'
#' From the original dataset, some fields have been selected and renamed, and
#' only includes the Mortality Reporting System states.
#'
#' @examples
#'
#' # Read by:
#' #
#' # filepath <- "data/cb_2018_us_state_20m/cb_2018_us_state_20m.shp"
#' # usa_states <- st_read(filepath)  |>
#' #   dplyr::select(geo_id = GEOID,
#' #                 state = STUSPS,
#' #                 state_name = NAME)
#'
#' @format A `sf`.
#' @source
#'   \url{https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_state_20m.zip}
"usa_states"

#' USA Divisions, 2018
#'
#' From the original dataset, some fields have been selected and renamed.
#'
#' @examples
#'
#' # Read by:
#' #
#' # filepath <- "data/cb_2018_us_division_20m/cb_2018_us_division_20m.shp"
#' # usa_divisions <- st_read(filepath)  |>
#' #   dplyr::select(geo_id = GEOID,
#' #                 division = NAME)
#'
#' @format A `sf`.
#' @source
#'   \url{https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_division_20m.zip}
"usa_divisions"

#' USA Regions, 2018
#'
#' From the original dataset, some fields have been selected and renamed.
#'
#' @examples
#'
#' # Read by:
#' #
#' # filepath <- "data/cb_2018_us_region_20m/cb_2018_us_region_20m.shp"
#' # usa_regions <- st_read(filepath)  |>
#' #   dplyr::select(geo_id = GEOID,
#' #                 region = NAME)
#'
#' @format A `sf`.
#' @source
#'   \url{https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_region_20m.zip}
"usa_regions"

#' USA Nation, 2018
#'
#' From the original dataset, some fields have been selected and renamed.
#'
#' @examples
#'
#' # Read by:
#' #
#' # filepath <- "data/cb_2018_us_nation_20m/cb_2018_us_nation_20m.shp"
#' # usa_nation <- st_read(filepath)  |>
#' #   dplyr::select(geo_id = GEOID,
#' #                 name = NAME)
#'
#' @format A `sf`.
#' @source
#'   \url{https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_nation_20m.zip}
"usa_nation"


#' UK London Boroughs
#'
#' From the original dataset, some fields have been selected and renamed.
#'
#' Since not so much detail is needed, the geometry has been simplified 20 m.
#'
#' @examples
#'
#' # Read by:
#' #
#' # filepath <- "data/London_Borough_Excluding_MHW/London_Borough_Excluding_MHW.shp"
#' # uk_london_boroughs <- st_read(filepath)  |>
#' #     dplyr::select(
#' #       borough = NAME,
#' #       gss_code = GSS_CODE
#' #     ) |>
#' #   st_simplify(dTolerance = 20)
#'
#' @format A `sf`.
#' @source
#'   \url{https://data.london.gov.uk/dataset/statistical-gis-boundary-files-london}
"uk_london_boroughs"

#' Dimension *when*
#'
#' *When* dimension table of the Mortality Reporting System. Defined from
#' `ms_mrs`. The primary key has been renamed and its type has been changed. The
#' other attributes have also been renamed.
#'
#' @format A `tibble`.
#' @source
#'   \url{https://CRAN.R-project.org/package=starschemar}
"mrs_when"

#' Dimension *where*
#'
#' *Where* dimension table of the Mortality Reporting System. Defined from
#' `ms_mrs`. The primary key has been renamed.
#'
#' @format A `tibble`.
#' @source
#'   \url{https://CRAN.R-project.org/package=starschemar}
"mrs_where"

#' Dimension *who*
#'
#' *Who* dimension table of the Mortality Reporting System. Defined from
#' `ms_mrs`. The primary key has been renamed.
#'
#' @format A `tibble`.
#' @source
#'   \url{https://CRAN.R-project.org/package=starschemar}
"mrs_who"

#' Fact *age*
#'
#' Fact *age* table of the Mortality Reporting System. Defined from `ms_mrs`.
#' Foreign keys have been renamed, only a *when* dimension has been considered,
#' the type for the *when* dimension has been changed.
#'
#' @format A `tibble`.
#' @source
#'   \url{https://CRAN.R-project.org/package=starschemar}
"mrs_fact_age"

#' Fact *cause*
#'
#' Fact *cause* table of the Mortality Reporting System. Defined from `ms_mrs`.
#' Foreign keys have been renamed, only a *when* dimension has been considered,
#' the type for the *when* dimension has been changed.
#'
#' @format A `tibble`.
#' @source
#'   \url{https://CRAN.R-project.org/package=starschemar}
"mrs_fact_cause"

