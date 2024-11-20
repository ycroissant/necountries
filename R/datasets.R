#' Countries of naturalearth
#' 
#' A `sf` containing 299 countries (in a large sense), either the main
#' parts of sovereign countries, parts or dependencies
#'
#' @name ne_countries
#' @docType data
#' @format a `sf` containing
#' - `iso2`: two letters identifier of the country,
#' - `iso3`: two letters identifier of the country,
#' - `type`: either `"main"` (the main part of a sovereign country, the
#' whole country for most of them), `"part"`, `"dependency"` or `"indeterminate"`
#' - `country`: the name of the entity,
#' - `sovereign`: the sovereign country the entity belongs to,
#' - `capital`: the name of the capital of the country (NA for parts
#' and dependencies)
#' - `status`: United Nations' status
#' - `en`, `fr`, `de`, `es`, `it`: the name of the country in
#' different languages
#' - `region`: the name of the region (United Nations' definition)
#' - `subregion`: the name of the subregion (United Nations'
#' definition)
#' - `wbregion`: the name of the region (World Bank's definition)
#' - `pop`: the population of the entity,
#' - `gdp`: currently undocumented
#' - `economy`: economic group
#' - `income`: income groupe
#' - `polygon` a geometry column containing the administrative borders
#' - `point` a geometry column containing the point coordinate of the
#' capital
#' @keywords datasets
NULL

#' Populated places of naturalearth
#' 
#' A `sf` containing 7342 cities
#'
#' @name ne_towns
#' @docType data
#' @format a `sf` containing
#' - `id`: the id of the  country,
#' - `name`: the name of the city,
#' - `capital`: a boolean, `TRUE` for a capital
#' - `pop`: the population of the city,
#' - `point`: a point `sfc` containing the coordinates of the city
#' @keywords datasets
NULL

#' Slave trade and economic development
#' @name slave_trade
#' @docType data
#' @keywords dataset
#' @description  a cross-section of 52 countries from 2000 
#' @format a tibble containing:
#' - country: the country name
#' - region: one of 'north', 'east', 'central', 'south', 'west'
#' - disteq: distance from equator
#' - longitude: longitude
#' - area: area in thousands of km squared
#' - pop: average population during the slave trade period
#' - coastline: log coastlines divided by the country area
#' - island: island indicator
#' - islam: percent islamic
#' - colony: previous colonizator, one of 'none', 'uk', 'france', 'portugal', 'belgium', 'spain', 'germany', 'italy'
#' - legor: legal origin, one of 'french' and 'british'
#' - gdp: log real gdp per capita in 2000
#' - slaves: number of slaves
#' - slavesarea: number of slaves divided by the country area
#' - humidmax: average maximum humidity
#' - rainmin: lowest month rainfall
#' - lowtemp: average minimum temperature
#' - gold: log gold production per inhabitant
#' - oil: log oil production per inhabitant
#' - diamond: log diamonds production per inhabitant
#' - atlantic: distance to the atlantic ocean
#' - indian: distance to the indian ocean
#' - redsea: distance to the red sea
#' - sahara: distance to sahara
#' @source Nathan Nunn's website \url{https://nathannunn.arts.ubc.ca/}
#' @references
#' \insertRef{NUNN:08}{necountries}
#' @importFrom Rdpack reprompt
NULL

#' Solow's growth model with spatial correlation
#' @name sp_solow
#' @docType data
#' @keywords dataset
#' @description  a cross-section of 91 countries from 1995 
#' @format a tibble containing:
#' - name: the name of the country
#' - code: the id of the country
#' - gdp60: per capita gdp in 1960
#' - gdp95: per capita gdp in 1995
#' - saving: saving rate
#' - labgwth: growth rate of the labor force
#' @source JAE data archive
#' @references
#' \insertRef{ERTU:KOCH:07}{necountries}
#' @importFrom Rdpack reprompt
NULL
