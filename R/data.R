
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
#' region <- c(1,
#'             1,
#'             2,
#'             2,
#'             3,
#'             3,
#'             3,
#'             4,
#'             4)
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
#' tb <- data.frame(
#'   state = c(
#'     "FL",
#'     "IL",
#'     "MN",
#'     "MD",
#'     "RI",
#'     "NC",
#'     "CT",
#'     "DE",
#'     "NM",
#'     "CA",
#'     "NJ",
#'     "WI",
#'     "OR",
#'     "NE",
#'     "PA",
#'     "WA",
#'     "LA",
#'     "GA",
#'     "AL",
#'     "UT",
#'     "OH",
#'     "TX",
#'     "CO",
#'     "OK",
#'     "TN",
#'     "HI",
#'     "NY",
#'     "MI",
#'     "AR",
#'     "MO",
#'     "KS",
#'     "IN",
#'     "MA",
#'     "VA",
#'     "DC",
#'     "IA",
#'     "AZ",
#'     "Unknown"
#'   ),
#'   state_name = c(
#'     "Florida",
#'     "Illinois",
#'     "Minnesota",
#'     "Maryland",
#'     "Rhode Island",
#'     "North Carolina",
#'     "Connecticut",
#'     "Delaware",
#'     "New Mexico",
#'     "California",
#'     "New Jersey",
#'     "Wisconsin",
#'     "Oregon",
#'     "Nebraska",
#'     "Pennsylvania",
#'     "Washington",
#'     "Louisiana",
#'     "Georgia",
#'     "Alabama",
#'     "Utah",
#'     "Ohio",
#'     "Texas",
#'     "Colorado",
#'     "Oklahoma",
#'     "Tennessee",
#'     "Hawaii",
#'     "New York",
#'     "Michigan",
#'     "Arkansas",
#'     "Missouri",
#'     "Kansas",
#'     "Indiana",
#'     "Massachusetts",
#'     "Virginia",
#'     "District of Columbia",
#'     "Iowa",
#'     "Arizona",
#'     "Unknown"
#'   )
#' )
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
#' tb <-
#'   data.frame(
#'     city = c(
#'       "Rochester",
#'       "Syracuse",
#'       "Buffalo",
#'       "Schenectady",
#'       "Lynn",
#'       "Albany",
#'       "Lowell",
#'       "Cambridge",
#'       "Worcester",
#'       "Boston",
#'       "Springfield",
#'       "Erie",
#'       "Chicago",
#'       "Fall River",
#'       "Toledo",
#'       "Cleveland",
#'       "Philadelphia",
#'       "Akron",
#'       "Paterson",
#'       "Canton",
#'       "Allentown",
#'       "Reading",
#'       "Pittsburgh",
#'       "Trenton",
#'       "Camden",
#'       "Baltimore",
#'       "Kansas City",
#'       "Saint Louis",
#'       "Wichita",
#'       "Richmond",
#'       "New Orleans",
#'       "Utica",
#'       "Newark",
#'       "Grand Rapids",
#'       "Milwaukee",
#'       "Somerville",
#'       "New Bedford",
#'       "Providence",
#'       "Scranton",
#'       "Yonkers",
#'       "Jersey City",
#'       "Elizabeth",
#'       "Norfolk",
#'       "Detroit",
#'       "Hartford",
#'       "Bridgeport",
#'       "New Haven",
#'       "Waterbury",
#'       "Wilmington",
#'       "Washington",
#'       "Des Moines",
#'       "Duluth",
#'       "Minneapolis",
#'       "Gary",
#'       "Fort Wayne",
#'       "New York",
#'       "Kansas City",
#'       "Evansville",
#'       "Saint Paul",
#'       "South Bend",
#'       "Omaha",
#'       "Lincoln",
#'       "Peoria",
#'       "Atlanta",
#'       "Birmingham",
#'       "Baton Rouge",
#'       "Charlotte",
#'       "Chattanooga",
#'       "Montgomery",
#'       "Mobile",
#'       "Jacksonville",
#'       "Miami",
#'       "Memphis",
#'       "Little Rock",
#'       "Shreveport",
#'       "Savannah",
#'       "Saint Petersburg",
#'       "Tampa",
#'       "Denver",
#'       "Colorado Springs",
#'       "Berkeley",
#'       "Albuquerque",
#'       "Dallas",
#'       "Austin",
#'       "Corpus Christi",
#'       "Portland",
#'       "Ogden",
#'       "Pueblo",
#'       "Fresno",
#'       "Glendale",
#'       "Long Beach",
#'       "Los Angeles",
#'       "Phoenix",
#'       "Fort Worth",
#'       "El Paso",
#'       "Houston",
#'       "Spokane",
#'       "Seattle",
#'       "Salt Lake City",
#'       "Sacramento",
#'       "San Francisco",
#'       "San Jose",
#'       "Tulsa",
#'       "San Diego",
#'       "Tucson",
#'       "San Antonio",
#'       "Indianapolis",
#'       "Honolulu",
#'       "Nashville",
#'       "Unknown"
#'     ),
#'     state = c(
#'       "NY",
#'       "NY",
#'       "NY",
#'       "NY",
#'       "MA",
#'       "NY",
#'       "MA",
#'       "MA",
#'       "MA",
#'       "MA",
#'       "MA",
#'       "PA",
#'       "IL",
#'       "MA",
#'       "OH",
#'       "OH",
#'       "PA",
#'       "OH",
#'       "NJ",
#'       "OH",
#'       "PA",
#'       "PA",
#'       "PA",
#'       "NJ",
#'       "NJ",
#'       "MD",
#'       "KS",
#'       "MO",
#'       "KS",
#'       "VA",
#'       "LA",
#'       "NY",
#'       "NJ",
#'       "MI",
#'       "WI",
#'       "MA",
#'       "MA",
#'       "RI",
#'       "PA",
#'       "NY",
#'       "NJ",
#'       "NJ",
#'       "VA",
#'       "MI",
#'       "CT",
#'       "CT",
#'       "CT",
#'       "CT",
#'       "DE",
#'       "DC",
#'       "IA",
#'       "MN",
#'       "MN",
#'       "IN",
#'       "IN",
#'       "NY",
#'       "MO",
#'       "IN",
#'       "MN",
#'       "IN",
#'       "NE",
#'       "NE",
#'       "IL",
#'       "GA",
#'       "AL",
#'       "LA",
#'       "NC",
#'       "TN",
#'       "AL",
#'       "AL",
#'       "FL",
#'       "FL",
#'       "TN",
#'       "AR",
#'       "LA",
#'       "GA",
#'       "FL",
#'       "FL",
#'       "CO",
#'       "CO",
#'       "CA",
#'       "NM",
#'       "TX",
#'       "TX",
#'       "TX",
#'       "OR",
#'       "UT",
#'       "CO",
#'       "CA",
#'       "CA",
#'       "CA",
#'       "CA",
#'       "AZ",
#'       "TX",
#'       "TX",
#'       "TX",
#'       "WA",
#'       "WA",
#'       "UT",
#'       "CA",
#'       "CA",
#'       "CA",
#'       "OK",
#'       "CA",
#'       "AZ",
#'       "TX",
#'       "IN",
#'       "HI",
#'       "TN",
#'       "Unknown"
#'     ),
#'     county = c(
#'       "Monroe",
#'       "Onondaga",
#'       "Erie",
#'       "Schenectady",
#'       "Essex",
#'       "Albany",
#'       "Middlesex",
#'       "Middlesex",
#'       "Worcester",
#'       "Suffolk",
#'       "Hampden",
#'       "Erie",
#'       "Cook",
#'       "Bristol",
#'       "Lucas",
#'       "Cuyahoga",
#'       "Philadelphia",
#'       "Summit",
#'       "Passaic",
#'       "Stark",
#'       "Lehigh",
#'       "Berks",
#'       "Allegheny",
#'       "Mercer",
#'       "Camden",
#'       "Baltimore",
#'       "Wyandotte",
#'       "St. Louis",
#'       "Sedgwick",
#'       "Richmond",
#'       "Orleans",
#'       "Oneida",
#'       "Essex",
#'       "Kent",
#'       "Milwaukee",
#'       "Middlesex",
#'       "Bristol",
#'       "Providence",
#'       "Lackawanna",
#'       "Westchester",
#'       "Hudson",
#'       "Union",
#'       "Norfolk",
#'       "Wayne",
#'       "Hartford",
#'       "Fairfield",
#'       "New Haven",
#'       "New Haven",
#'       "New Castle",
#'       "District of Columbia",
#'       "Polk",
#'       "St. Louis",
#'       "Hennepin",
#'       "Lake",
#'       "Allen",
#'       "New York",
#'       "Jackson",
#'       "Vanderburgh",
#'       "Ramsey",
#'       "St. Joseph",
#'       "Douglas",
#'       "Lancaster",
#'       "Peoria",
#'       "Fulton",
#'       "Jefferson",
#'       "East Baton Rouge",
#'       "Mecklenburg",
#'       "Hamilton",
#'       "Montgomery",
#'       "Mobile",
#'       "Duval",
#'       "Miami-Dade",
#'       "Shelby",
#'       "Pulaski",
#'       "Caddo",
#'       "Chatham",
#'       "Pinellas",
#'       "Hillsborough",
#'       "Denver",
#'       "El Paso",
#'       "Alameda",
#'       "Bernalillo",
#'       "Dallas",
#'       "Travis",
#'       "Nueces",
#'       "Multnomah",
#'       "Weber",
#'       "Pueblo",
#'       "Fresno",
#'       "Los Angeles",
#'       "Los Angeles",
#'       "Los Angeles",
#'       "Maricopa",
#'       "Tarrant",
#'       "El Paso",
#'       "Harris",
#'       "Spokane",
#'       "King",
#'       "Salt Lake",
#'       "Sacramento",
#'       "San Francisco",
#'       "Santa Clara",
#'       "Tulsa",
#'       "San Diego",
#'       "Pima",
#'       "Bexar",
#'       "Marion",
#'       "Honolulu",
#'       "Davidson",
#'       "Unknown"
#'     )
#'   )
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
#' From the original dataset, some fields have been selected and renamed.
#'
#' @format A `sf`.
#' @source
#'   \url{https://earthworks.stanford.edu/catalog/stanford-bx729wr3020}
"usa_cities"


#' USA Counties, 2018
#'
#' From the original dataset, some fields have been selected and renamed.
#'
#' @format A `sf`.
#' @source
#'   \url{https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_county_20m.zip}
"usa_counties"


#' USA States, 2018
#'
#' From the original dataset, some fields have been selected and renamed.
#'
#' @format A `sf`.
#' @source
#'   \url{https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_state_20m.zip}
"usa_states"

#' USA Divisions, 2018
#'
#' From the original dataset, some fields have been selected and renamed.
#'
#' @format A `sf`.
#' @source
#'   \url{https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_division_20m.zip}
"usa_divisions"

#' USA Regions, 2018
#'
#' From the original dataset, some fields have been selected and renamed.
#'
#' @format A `sf`.
#' @source
#'   \url{https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_region_20m.zip}
"usa_regions"

#' USA Nation, 2018
#'
#' From the original dataset, some fields have been selected and renamed.
#'
#' @format A `sf`.
#' @source
#'   \url{https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_nation_20m.zip}
"usa_nation"

