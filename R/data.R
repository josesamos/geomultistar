#' Multistar for Mortality Reporting System with the Enriched `where` Dimension
#'
#' Multistar for the Mortality Reporting System considering age and cause
#' classification with the enriched `where` dimension.
#'
#' @examples
#' # Defined by:
#'
#' library(tidyr)
#' library(starschemar)
#'
#' where <- st_mrs_age %>%
#'   get_dimension("where")
#'
#' updates <- record_update_set() %>%
#'   update_selection(
#'     dimension = where,
#'     columns = c("city"),
#'     old_values = c("Bridgepor"),
#'     new_values = c("Bridgeport")
#'   ) %>%
#'   update_selection(
#'     dimension = where,
#'     columns = c("city"),
#'     old_values = c("Wilimington"),
#'     new_values = c("Wilmington")
#'   )
#'
#' st_mrs_age <-
#'   st_mrs_age %>%
#'   modify_dimension_records(updates)
#'
#' st_mrs_cause <-
#'   st_mrs_cause %>%
#'   modify_dimension_records(updates)
#'
#'
#' st_mrs_age <-
#'   st_mrs_age %>%
#'   rename_dimension_attributes(
#'     name = "where",
#'     attributes = c("region"),
#'     new_names = c("division")
#'   )
#'
#' st_mrs_cause <-
#'   st_mrs_cause %>%
#'   rename_dimension_attributes(
#'     name = "where",
#'     attributes = c("region"),
#'     new_names = c("division")
#'   )
#'
#' tb <-
#'   enrich_dimension_export(st_mrs_age,
#'                           name = "where",
#'                           attributes = c("division"))
#'
#' division_name <- c(
#'   "New England",
#'   "Middle Atlantic",
#'   "East North Central",
#'   "West North Central",
#'   "South Atlantic",
#'   "East South Central",
#'   "West South Central",
#'   "Mountain",
#'   "Pacific"
#' )
#'
#' region_name <- c(
#'   "Northeast",
#'   "Northeast",
#'   "Midwest",
#'   "Midwest",
#'   "South",
#'   "South",
#'   "South",
#'   "West",
#'   "West"
#' )
#'
#' region <- c('1',
#'             '1',
#'             '2',
#'             '2',
#'             '3',
#'             '3',
#'             '3',
#'             '4',
#'             '4')
#'
#' tb <-
#'   tibble::add_column(tb,
#'                      division_name = division_name,
#'                      region = region,
#'                      region_name = region_name)
#'
#' st_mrs_age <-
#'   st_mrs_age %>%
#'   enrich_dimension_import(name = "where", tb)
#'
#' st_mrs_cause <-
#'   st_mrs_cause %>%
#'   enrich_dimension_import(name = "where", tb)
#'
#' tb <- ft_usa_states %>%
#'    tibble::add_row(state = "Unknown", state_name = "Unknown")
#'
#' st_mrs_age <-
#'   st_mrs_age %>%
#'   enrich_dimension_import(name = "where", tb)
#'
#' st_mrs_cause <-
#'   st_mrs_cause %>%
#'   enrich_dimension_import(name = "where", tb)
#'
#' tb <-
#'   enrich_dimension_export(st_mrs_age,
#'                           name = "where",
#'                           attributes = c("city", "state"))
#'
#' tb <- ft_usa_city_county_mrs %>%
#'   tibble::add_row(city = "Unknown",
#'                   state = "Unknown",
#'                   county = "Unknown")
#'
#' st_mrs_age <-
#'   st_mrs_age %>%
#'   enrich_dimension_import(name = "where", tb)
#'
#' st_mrs_cause <-
#'   st_mrs_cause %>%
#'   enrich_dimension_import(name = "where", tb)
#'
#' ms_where <- constellation(list(st_mrs_age, st_mrs_cause), name = "mrs") %>%
#'   constellation_as_multistar()
#'
#' @format A `multistar` object.
#' @source \url{https://cran.r-project.org/package=starschemar}
"ms_where"


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
#' # usa_cities <- st_read(filepath)  %>%
#' #   dplyr::select(
#' #     gnis_id = GNIS_ID,
#' #     ansi_code = ANSICODE,
#' #     city = NAME,
#' #     state = STATE,
#' #     state_fp = STATE_FIPS,
#' #     county_fp = COUNTYFIPS,
#' #     county = COUNTY,
#' #     latitude = LATITUDE,
#' #     longitude = LONGITUDE,
#' #     elev_m = ELEV_IN_M
#' #   )  %>%
#' #   dplyr::mutate(
#' #     county_geo_id = paste(state_fp, county_fp, sep = "", collapse = NULL),
#' #     .after = county_fp
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
#' # usa_counties <- st_read(filepath)  %>%
#' #   dplyr::select(
#' #     geo_id = GEOID,
#' #     state_fp = STATEFP,
#' #     county_fp = COUNTYFP,
#' #     county = NAME
#' #   )
#' # states <- sf::st_drop_geometry(usa_states[, c("geo_id", "state")])
#' # usa_counties <- usa_counties %>%
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
#' # usa_states <- st_read(filepath)  %>%
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
#' # usa_divisions <- st_read(filepath)  %>%
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
#' # usa_regions <- st_read(filepath)  %>%
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
#' # usa_nation <- st_read(filepath)  %>%
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
#' # uk_london_boroughs <- st_read(filepath)  %>%
#' #     dplyr::select(
#' #       borough = NAME,
#' #       gss_code = GSS_CODE
#' #     ) %>%
#' #   st_simplify(dTolerance = 20)
#'
#' @format A `sf`.
#' @source
#'   \url{https://data.london.gov.uk/dataset/statistical-gis-boundary-files-london}
"uk_london_boroughs"

