#https://github.com/lukes/ISO-3166-Countries-with-Regional-Codes
## source("./R/functions.R") ; load("./R/sysdata.rda") ; load("./data/ne_countries.rda"); load("./data/ne_towns.rda") ; library(sf) ; library(dplyr)
library(tidyverse)
library(sf)
sf_use_s2(FALSE)
#old_options <- options(warn = -1)
bb_bbox <- function(x, crs = 4326){
    v <- st_bbox(x)
    bb(v[1], v[3], v[2], v[4], crs = crs)
}

bb <- function(long_min, long_max, lat_min, lat_max, crs = 4326){
    pts <- matrix(c(long_min, long_min, long_max, long_max, long_min,
                    lat_min, lat_max, lat_max, lat_min, lat_min),
                  ncol = 2)
    st_polygon(list(pts)) %>% st_sfc(crs = crs) 
}

# 1. Reading the different files

## Reading the 249 official codes
iso <- read_csv("../inst/extdata/iso_codes.csv") %>%
    mutate(`alpha-2` = ifelse(name == "Namibia", "NA", `alpha-2`)) %>%
    select(name, id = 2)

## United Nations' countries
un_nations <- read_csv("../inst/extdata/united_nations.csv") %>% filter(UN == 1) %>% pull(country)
un_nsgt <- c("American Samoa", "Anguilla", "Bermuda",
             "British Virgin Islands", "Cayman Islands", "Falkland Islands",
             "French Polynesia", "Gibraltar", "Guam", "Montserrat",
             "New Caledonia", "Pitcairn Islands", "Saint Helena",
             "Tokelau", "Turks and Caicos Islands", "United States Virgin Islands",
             "Western Sahara")
un_os <- c("Vatican", "Palestine")
un_enms <- c("Cook Islands", "Niue")
united_nations <- tibble(country = c(un_nations, un_os, un_enms, un_nsgt),
                         status = c(rep("member", length(un_nations)),
                                    rep("observer", 2),
                                    rep("eligible", 2),
                                    rep("nonselfgov", length(un_nsgt))))

## Sovereign shape
sovereign <- st_read("../inst/extdata/ne_10m_admin_0_sovereignty/ne_10m_admin_0_sovereignty.shp") %>%
    select(iso2 = ISO_A2, type = TYPE, country = ADMIN,
           sovereign = SOVEREIGNT, region = REGION_UN, subregion = SUBREGION,
           wbregion = REGION_WB) %>%
    mutate(iso2 = ifelse(iso2 == -99, NA, iso2))
Spain <- sovereign %>% filter(country == "Spain")
sovereignty_list <- sovereign %>%
    as_tibble %>%
    select(iso2, country, type, sovereign) %>%
    mutate(un = ifelse(country %in% c("Kosovo", "Antarctica", "Bajo Nuevo Bank (Petrel Is.)",
                                      "Bir Tawil", "Brazilian Island", "Cyprus No Mans Area",
                                      "Scaraborough Reef", "Serranilla Bank", "Siachen Glacier",
                                      "Southern Patagonian Ice Field", "Spratly Islands",
                                      "Western Sahara", "Northern Cyprus", "Somaliland",
                                      "Taiwan", "Vatican"), "no", "yes"))

## Units and Subunits' shape
units <- st_read("../inst/extdata/ne_10m_admin_0_map_units/ne_10m_admin_0_map_units.shp") %>%
    select(iso2 = ISO_A2, type = TYPE, country = ADMIN,
           sovereign = SOVEREIGNT, region = REGION_UN, subregion = SUBREGION,
           pop = POP_EST, gdp = GDP_MD, wbregion = REGION_WB) %>%
    mutate(iso2 = ifelse(iso2 == -99, NA, iso2))
subunits <- st_read("../inst/extdata/ne_10m_admin_0_map_subunits/ne_10m_admin_0_map_subunits.shp") %>%
    select(iso2 = ISO_A2, type = TYPE, country = ADMIN,
           sovereign = SOVEREIGNT, region = REGION_UN, subregion = SUBREGION,
           pop = POP_EST, gdp = GDP_MD, wbregion = REGION_WB,
           en = NAME_EN, fr = NAME_FR, de = NAME_DE, es = NAME_ES, it = NAME_IT) %>%
    mutate(iso2 = ifelse(iso2 == -99, NA, iso2))

## Countries' shape
countries <- st_read("../inst/extdata/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") %>%
        select(iso2 = ISO_A2, iso3 = ISO_A3, type = TYPE, country = ADMIN,
               sovereign = SOVEREIGNT, region = REGION_UN, subregion = SUBREGION,
               pop = POP_EST, gdp = GDP_MD, economy = ECONOMY, income = INCOME_GRP,
               wbregion = REGION_WB, en = NAME_EN, fr = NAME_FR, de = NAME_DE, es = NAME_ES, it = NAME_IT) %>%
    mutate(iso2 = ifelse(iso2 == -99, NA, iso2),
           iso3 = ifelse(iso3 == "-99", NA, iso3),
           iso2 = ifelse(iso2 == "CN-TW", "TW", iso2),
           iso2 = ifelse(country == "Namibia", "NA", iso2),
           iso2 = ifelse(country == "France", "FR", iso2),
           iso2 = ifelse(country == "Norway", "NO", iso2),
           iso3 = ifelse(country == "France", "FRA", iso3),
           iso3 = ifelse(country == "Norway", "NOR", iso3),
           region = ifelse(region == "Antarctica", NA, region),
           subregion = ifelse(subregion %in% c("Seven seas (open ocean)", "Antarctica"), NA, subregion),
           country = ifelse(country == "São Tomé and Principe", "Sao Tome and Principe", country),
           sovereign = ifelse(sovereign == "São Tomé and Principe", "Sao Tome and Principe", sovereign),
           iso2 = ifelse(country == "Kosovo", "XK", iso2),
           type = ifelse(country %in% c("China", "Israel", "Kazakhstan", "Finland", "Kosovo", "Cuba",
                                        "Denmark", "Netherlands", "France", "New Zealand", "Australia",
                                        "United States of America", "United Kingdom"), "Sovereign country", type),
           type = ifelse(country %in% c("Hong Kong S.A.R.", "Macao S.A.R"), "Dependency", type),
           type = ifelse(type %in% c("Country", "Lease", "Disputed"), "Dependency", type),
           type = ifelse(country == "Palestine", "Sovereign country", type),
           subregion = ifelse(country == "Saint Helena", NA, subregion),
           income = ifelse(income == "-99", NA, income),
           economy = ifelse(economy == "-99", NA, economy))
countries <- countries %>% left_join(united_nations, by = "country")
countries_list <- countries %>% as_tibble %>% select(iso2, country, type, sovereign)

## List of dependencies (diff between countries and sovereignties
deps <- anti_join(countries_list, sovereignty_list, by = "country") %>%
    select(country, sovereign) %>% nest(.by = sovereign)
names_deps <- deps[[1]]
deps <- map(deps[[2]], ~ paste(.x[[1]], collapse = ", "))
names(deps) <- names_deps

# 2. Split of some countries
decomp_countries <- countries %>%
    filter(country %in% c("France", "Netherlands", "Norway",
                          "Spain", "Portugal", "Russia",
                          "United States of America",
                          "Mauritius", "New Zealand", "Chile",
                          "South Africa", "Brazil", "Denmark",
                          "United Kingdom", "Ecuador", "Colombia",
                          "Venezuela", "Equatorial Guinea",
                          "Australia", "Japan", "Seychelles",
                          "Antigua and Barbuda")) %>%# , "Indian Ocean Territories")) %>%
    mutate(iso2 = ifelse(country == "France", "FR", iso2),
           iso2 = ifelse(country == "Norway", "NO", iso2))
decomp_countries_ids <- decomp_countries %>% pull(iso2)
reg_subregs <- countries %>% as_tibble %>%
    filter(! is.na(subregion)) %>%
    select(region, subregion) %>% distinct
countries <- countries %>% select(- region)

## 2.1 Simplified shapes

### Brazil
brazil <- countries %>% filter(country == "Brazil") %>% st_geometry
islands <- bb(-34, -25, -20, 10)
bb_brazil <- bb(-80, -34, -40, 10)
brazilian_islands <- st_intersection(brazil, islands)
brazil_main <- st_difference(brazil, islands)
brazil_main <- st_intersection(brazil, bb_brazil)
### United Kingdom
uk <- countries %>% filter(country == "United Kingdom") %>% st_geometry
uk_main <- st_intersection(uk, bb(-10, 5, 48, 65))
uk_islands <- st_difference(uk, uk_main)
### Japan
japan <- countries %>% filter(country == "Japan") %>% st_geometry
japan_island <- st_intersection(japan, bb(150, 155, 24, 25))
japan_main <- st_difference(japan, japan_island)
japan_decomp <- tibble(iso2 = c("JP"),
                       sovereign = "Japan",
                       type = "Sovereign country",
                       country = c("Japan"),
                       subregion = "Eastern Asia",
                       geometry = japan_main)
### Venezuela
venezuela <- countries %>% filter(country == "Venezuela") %>% st_geometry
venezuela_main <- st_intersection(venezuela, bb(-74, - 58, 0, 13))
venezuela_islands <- st_difference(venezuela, venezuela_main)

## 2.2 Shapes splited in two

### Indian Ocean Territories
iot <- countries %>% filter(country == "Indian Ocean Territories") %>% st_geometry
cocos <- st_intersection(iot, bb(90, 100, -12, -13))
christmas <- st_intersection(iot, bb(105, 108, -11, -10))
iot_decomp <- tibble(iso2 = c("CX", "CC"),
                     type = rep("Dependency", 2),
                     country = c("Christmas Island", "Cocos Islands"),
                     sovereign = "Australia",
                     subregion = NA,
                     geometry = c(christmas, cocos))
### United States Minor Outlying Islands
usa_minor_outlying <- countries %>% filter(country == "United States Minor Outlying Islands") %>% st_geometry
navassa_bbox <- bb(-120, -60, 0, 100)
navassa <- st_intersection(usa_minor_outlying, navassa_bbox)
usa_minor_outlying <- st_difference(usa_minor_outlying, navassa_bbox)
usa_moi_decomp <- tibble(iso2 = rep(NA, 2),
                         country = c("United States Minor Outlying Islands", "Navassa"),
                         sovereign = rep("United States of America", 2),
                         type = rep("Dependency", 2),
                         subregion = rep(NA, 2),
                         geometry = c(usa_minor_outlying, navassa))

## 2.3 Parts' extractions

### France
france_metro_bbox <- bb(-10, 10, 40, 55)
reunion_bbox <- bb(40, 60, -20, -30)
mayotte_bbox <- bb(40, 60, -10, -20)
guyane_bbox <- bb(-60, -40, -20, 10)
guadeloupe_bbox <- bb(-90, -60, 30, 15)
martinique_bbox <- bb(-90, -60, 15, 10)
france <- countries %>% filter(country == "France") %>% st_geometry
france_metro <- st_intersection(france, france_metro_bbox)
reunion <- st_intersection(france, reunion_bbox)
mayotte <- st_intersection(france, mayotte_bbox)
guyane <- st_intersection(france, guyane_bbox)
guadeloupe <- st_intersection(france, guadeloupe_bbox)
martinique <- st_intersection(france, martinique_bbox)
fr_decomp <- tibble(iso2= c("RE", "YT", "GF", "MQ", "GP"),
                    type = rep("part", 5),
                    sovereign = "France",
                    country = c("Reunion", "Mayotte", "Guyane", "Martinique", "Guadeloupe"),
                    subregion = rep(NA, 5),
                    geometry = c(reunion, mayotte, guyane, martinique, guadeloupe))
### United States
usa <- countries %>% filter(country == "United States of America") %>% st_geometry
usa_bbox <- bb(-130, -60, -20, 50)
usa_continent <- st_intersection(usa, usa_bbox)
daub <- st_difference(usa, usa_bbox)
alaska_bbox <- bb(- 180, - 130, 50, 80)
alaska <- st_intersection(usa, alaska_bbox)
far_west_usa_bbox <- bb(150, 180, 40, 80) 
far_west_usa <- st_intersection(usa, far_west_usa_bbox)
alaska <- st_union(alaska, far_west_usa)
hawaii_bbox <- bb(- 180, - 140, 10, 40)
hawaii <- st_intersection(usa, hawaii_bbox)
usa_decomp <- tibble(iso2= rep(NA, 2),#"US-AK", "US-HI"),
                     country = c("Alaska", "Hawaii"),
                     sovereign = rep("United States of America", 2),
                     type = rep("part", 2),
                     subregion = c("Northern America", "Polynesia"),
                     geometry = c(alaska, hawaii))
### Norway
norway <- countries %>% filter(country == "Norway") %>% st_geometry
norway_bbox <- bb(-40, 50, 0, 90)
norway_metro <- st_intersection(norway, norway_bbox)
bouvet <- st_difference(norway, norway_bbox)
norway_metro_2 <- bb(0, 40, 50, 72)
norway_metro_2 <- st_intersection(norway, norway_metro_2)
norway_islands <- st_difference(norway_metro, norway_metro_2)
norway_decomp <- tibble(iso2= c("BV", "SJ"),
                        country = c("Bouvet", "Svalbard and Jan Mayen"),
                        sovereign = rep("Norway", 2),
                        type = c("part", "part"),
                        subregion = rep(NA, 2),
                        geometry = c(bouvet, norway_islands))
### Netherland
netherlands_metro_bbox <- bb(0, 30, 30, 70)
netherlands <- countries %>% filter(country == "Netherlands") %>% st_geometry
netherlands_metro <- st_intersection(netherlands, netherlands_metro_bbox)
netherlands_part <- st_difference(netherlands, netherlands_metro_bbox)
nl_decomp <- tibble(iso2= c("BQ"),
                    sovereign = "Netherlands",
                    type = "part",
                    country = "Bonaire, Sint Eustatius and Saba",
                    subregion = NA,
                    geometry = netherlands_part)
### Portugal
ascores_bbox <- bb(- 35, - 20, 30, 40)
madere_bbox <- bb(-20, -10, 30, 40)
portugal_bbox <- bb(-10, -5, 36, 43)
portugal <- countries %>% filter(country == "Portugal") %>% st_geometry
ascores <- st_intersection(portugal, ascores_bbox)
madere <- st_intersection(portugal, madere_bbox)
portugal <- st_intersection(portugal, portugal_bbox)
portugal_decomp <- tibble(iso2= rep(NA, 2),#"PT-20", "PT-30"),
                          type = rep("part", 2),
                          country = c("Azores", "Madeira"),
                          sovereign = rep("Portugal", 2),
                          subregion = rep(NA, 2),
                          geometry = c(ascores, madere))
### Spain
canaries_bbox <- bb(-20, -10, 20, 30)
spain_bbox <- bb(-10, 5, 35, 45)
spain <- countries %>% filter(country == "Spain") %>% st_geometry
canaries <- st_intersection(spain, canaries_bbox)
spain <- st_intersection(spain, spain_bbox)
spain_decomp <- tibble(iso2= NA,#"IC"),
                       sovereign = "Spain",
                       country = "Cannaries",
                       type = "part",
                       subregion = NA,
                       geometry = canaries)
### Mauritius
mauritius <- countries %>% filter(country == "Mauritius") %>% st_geometry
agalega_bbox <- bb(56, 57, -12, -10)
agalega <- st_intersection(mauritius, agalega_bbox)
rodrigues_bbox <- bb(63, 64, -20, -18)
daub_bbox <- bb(55, 59, -12, -10)
rodrigues <- st_intersection(mauritius, rodrigues_bbox)
mauritius <- mauritius %>% st_difference(agalega_bbox) %>% st_difference(rodrigues_bbox)
mauritius_decomp <- tibble(iso2 = rep(NA, 2),
                           sovereign = rep("Mauritius", 2),
                           country = c("Rodrigues", "Agalega"),
                           subregion = c(NA, NA),
                           geometry = c(rodrigues, agalega),
                           type = c("part", "part"))
### New Zealand
new_zealand <- countries %>% filter(country == "New Zealand") %>% st_geometry
nz_bbox <- bb(160, 180, - 30, - 48)
nz <- new_zealand %>% st_intersection(nz_bbox)
nz_dependence <- new_zealand %>% st_difference(nz_bbox)
auckland <- new_zealand %>% st_intersection(bb(164, 168, -50, -55))
nz <- nz %>% st_difference(auckland)
tokelau <- new_zealand %>% st_intersection(bb(-171, -173, -8, - 10))
#nz_misc <- nz_dependence %>% st_difference(tokelau)
chatman <- new_zealand %>% st_intersection(bb(-178, - 176, -42, -45))
campbell <- new_zealand %>% st_intersection(bb(165, 178, -60, -50))
antipodes <- new_zealand %>% st_intersection(bb(178, 180, -50, -48))
kermadec <- new_zealand %>% st_intersection(bb(-178, - 176, -42, -45))
nz <- nz %>% st_difference(antipodes) %>% st_difference(chatman) %>% st_difference(auckland) %>%
    st_difference(campbell) %>% st_difference(tokelau) %>% st_difference(kermadec)
nz_decomp <- tibble(iso2 = c("TK", rep(NA, 5)),
                    country = c("Tokelau", "Chatman", "Kermadec", "Auckland", "Campbell", "Antipodes"),
                    sovereign = rep("New Zealand", 6),
                    geometry = c(tokelau, chatman, kermadec, auckland, campbell, antipodes),
                    type = rep("part", 6),
                    subregion = rep(NA, 6))
### Chile
chile <- countries %>% filter(country == "Chile") %>% st_geometry
paques_bbox <- bb(-110, -100, -30, -20)
paques <- st_intersection(chile, paques_bbox)
chile_main <- st_difference(chile, paques)
chile_decomp <- tibble(iso2 = NA,
                       type = "part",
                       country = "Easter Island",
                       sovereign = "Chile",
                       geometry = paques,
                       subregion = "Polynesia")
### Colombia
# petite ile dans le pacifique introuvable
colombia <- countries %>% filter(country == "Colombia") %>% st_geometry
santandres <- st_intersection(colombia, bb(-82, -81, 10, 15))
colombia_main <- st_intersection(colombia, bb(-79.5, -66, -5, 15))
colombia_decomp <- tibble(iso2 = NA,
                          type = "part",
                          country = "San Andres, Providencia and Santa Catalina",
                          sovereign = "Colombia",
                          subregion = NA,
                          geometry = c(santandres))
### South Africa
south_africa <- countries %>% filter(country == "South Africa") %>% st_geometry
marion <- bb(35, 40, -45, -50)
marion <- st_intersection(south_africa, marion)
south_africa_main <- st_difference(south_africa, marion)
south_africa_decomp <- tibble(iso2 = NA,
                              type = "part",
                              country = "Prince Edward Islands",
                              sovereign = "South Africa",
                              geometry = marion,
                              subregion = NA)
### Ecuador
equador <- countries %>% filter(country == "Ecuador") %>% st_geometry
galapagos <- st_intersection(equador, bb(- 85, - 95, -2, 2))
equador_main <- st_difference(equador, galapagos)
equador_decomp <- tibble(iso2 =  NA,
                         type = "part",
                         country = "Galapagos",
                         sovereign = "Ecuador",
                         subregion = NA,
                         geometry = galapagos)

### Australia
australia <- countries %>% filter(country == "Australia") %>% st_geometry
maquarie <- st_intersection(australia, bb(150, 160, -50, -60))
australia_main <- st_difference(australia, maquarie)
australia_decomp <- tibble(iso2 = NA,
                           type = "part",
                           country = "Macquarie Island",
                           sovereign = "Australia",
                           subregion = NA,
                           geometry = maquarie)
### Denmark
denmark <- countries %>% filter(country == "Denmark") %>% st_geometry
bornholm <- st_intersection(denmark, bb(14, 16, 54, 56))
denmark_main <- st_difference(denmark, bornholm)
denmark_decomp <- tibble(iso2 = NA,
                         type = "part",
                         country = "Bornholm",
                         sovereign = "Denmark",
                         subregion = NA,
                         geometry = bornholm)
### Equatorial Guinea
eqg <- countries %>% filter(country == "Equatorial Guinea") %>% st_geometry
annobon <- st_intersection(eqg, bb(5, 6, -2, -1))
eqg_main <- st_difference(eqg, annobon)
eqg_decomp <- tibble(iso2 = NA ,
                     type = "part",
                     country = "Annobon",
                     sovereign = "Equatorial Guinea",
                     subregion = NA,
                     geometry = annobon)
### Seychelles
seychelles <- countries %>% filter(country == "Seychelles") %>% st_geometry
aldabra <- st_intersection(seychelles, bb(46, 48, -10, -9))
coetivy <- st_intersection(seychelles, bb(56, 57, -7, - 7.5))
alphonse <- st_intersection(seychelles, bb(52, 53, -7.5, - 6.5))
attol_saint_joseph <- st_intersection(seychelles, bb(53, 53.5, -5.5, -5))
platte <- st_intersection(seychelles, bb(55.3, 55.4, -6, -5.5))
denis <- st_intersection(seychelles, bb(55.6, 55.7 , -3.7, -4))
seychelles_main <- seychelles %>% st_difference(aldabra) %>%
    st_difference(coetivy) %>%
    st_difference(alphonse) %>%
    st_difference(attol_saint_joseph) %>%
    st_difference(platte) %>%
    st_difference(denis)
seychelles_decomp <- tibble(iso2 = rep(NA, 6),
                            type = rep("part", 6),
                            country = c("Aldabra", "Coetivy",
                                        "Alphonse", "Attol Saint-Joseph",
                                        "Platte", "Denis"),
                            sovereign = "Seychelles",
                            subregion = rep(NA, 6),
                            geometry = c(aldabra, coetivy, alphonse,
                                         attol_saint_joseph, platte, denis))
### Antigua and Barbuda
antigua_barbuda <- countries %>% filter(country == "Antigua and Barbuda") %>% st_geometry
redonda <- st_intersection(antigua_barbuda, bb(-62.4, -62.3, 16, 17))
antigua_barbuda_main <- st_difference(antigua_barbuda, redonda)
antigua_barbuda_decomp <- tibble(iso2 = NA ,type = "part", country = "Redonda",
                                 sovereign = "Antigua and Barbuda",   subregion = NA,
                                 geometry = redonda)

## 2.4 Addition of parts to the countries' sf
### Modification of main parts' sfg
countries_names <- c("France", "United States of America",
                     "Norway", "Netherlands", "Portugal",
                     "Spain", "Mauritius", "New Zealand",
                     "Chile", "Colombia", "South Africa",
                     "Brazil", "United Kingdom", "Ecuador",
                     "Venezuela", "Australia", "Japan",
                     "Denmark", "Equatorial Guinea",
                     "Seychelles", "Antigua and Barbuda")
countries_geometries <-  c(france_metro, usa_continent, norway_metro_2,
                           netherlands_metro, portugal, spain, mauritius,
                           nz, chile_main, colombia_main, south_africa_main,
                           brazil_main, uk_main, equador_main, venezuela_main,
                           australia_main, japan_main, denmark_main, eqg_main,
                           seychelles_main, antigua_barbuda_main)
for (i in 1:length(countries_names))
    countries$geometry[countries$country == countries_names[i]] <- 
        countries_geometries[i]

countries <- countries %>%
    filter(! country %in% c("United States Minor Outlying Islands",
                            "Indian Ocean Territories")) %>%
    bind_rows(fr_decomp) %>%
    bind_rows(nl_decomp) %>%
    bind_rows(norway_decomp) %>%
    bind_rows(usa_moi_decomp) %>% 
    bind_rows(spain_decomp) %>%
    bind_rows(portugal_decomp) %>%
    bind_rows(usa_decomp) %>%
    bind_rows(mauritius_decomp) %>%
    bind_rows(nz_decomp) %>%
    bind_rows(chile_decomp) %>%
    bind_rows(south_africa_decomp) %>%
    bind_rows(denmark_decomp) %>%
    bind_rows(equador_decomp) %>%
    bind_rows(colombia_decomp) %>%
    bind_rows(eqg_decomp) %>%
    bind_rows(australia_decomp) %>%
    bind_rows(seychelles_decomp) %>%
    bind_rows(antigua_barbuda_decomp) %>%
    bind_rows(iot_decomp)

countries <- countries %>%
    mutate(type = ifelse(country %in% c("Macao S.A.R", "Hong Kong S.A.R."), "part", type))

## # Check for inconsitencies
## cou <- countries %>% as_tibble %>% select(iso2, country, type)
## full_join(cou, iso, by = c("iso2" = "id")) %>% filter(is.na(name)) %>% print(n = Inf)
## full_join(cou, iso, by = c("iso2" = "id")) %>% filter(is.na(country)) %>% print(n = Inf)

# 48 11 199

countries <- countries %>%
    mutate(type = factor(type,
                         levels = c("Sovereign country", "Dependency", "part", "Indeterminate"),
                         labels = c("main", "dependency", "part", "indeterminate"))) %>%
    rename(polygon = geometry) %>%
    mutate(sovereign = ifelse(type == "indeterminate", NA, sovereign))

# unknown ids, only Kosovo
setdiff(na.omit(countries$iso2), iso$id)


# 3. Gessing subregions of parts from the bbox of subregions
subregs <- countries %>%
    filter(! is.na(subregion), subregion != "Antarctica") %>% group_by(subregion) %>% summarise()
subregions <- subregs %>% pull(subregion)
shift_indexes <- c(1, 7:9, 12, 14)
shift_names <- subregions[shift_indexes]
init_names <- subregions[- shift_indexes]
subregs_init <- subregs %>% slice(- shift_indexes)
subregs_shift <- subregs %>% slice(shift_indexes) %>% st_shift_longitude
bbox <- lapply(st_geometry(subregs_init), bb_bbox) %>% Reduce(f = "c")
subregs_init <- subregs_init %>% add_column(bbox = bbox)
bbox <- lapply(st_geometry(subregs_shift), bb_bbox) %>% Reduce(f = "c")
subregs_shift <- subregs_shift %>% add_column(bbox = bbox)
parts <- countries %>% filter(type != "main", is.na(subregion))
parts_shift <- parts %>% st_shift_longitude
subreg_parts_init <- vector(length = 16, mode = "list")
subreg_parts_shift <- vector(length = 6, mode = "list")
for (i in 1:16) subreg_parts_init[[i]] <- parts[subregs_init$bbox[i], ] %>% pull(country)
for (i in 1:6) subreg_parts_shift[[i]] <- parts_shift[subregs_shift$bbox[i], ] %>% pull(country)
names(subreg_parts_init) <- init_names
names(subreg_parts_shift) <- shift_names
subreg_parts <- c(subreg_parts_init, subreg_parts_shift)
for (i in 1:length(subreg_parts)){
    if (length(subreg_parts[[i]])){
        for (j in subreg_parts[[i]]) countries$subregion[countries$country == j] <- names(subreg_parts[i])
    }
}
countries$subregion[countries$country %in% c("Azores", "Madeira", "Cannaries")] <- "Northern Africa"
countries$subregion[countries$country %in% "Bornhom"] <- "Northern Europe"
countries$subregion[countries$country %in% "San Andres, Providencia and Santa Catalina"] <- "Central America"
countries$subregion[countries$country %in% "Tokelau"] <- "Micronesia"
countries$subregion[countries$country %in% "Kiribati"] <- "Polynesia"
countries$subregion[countries$country %in% "Rodrigues"] <- "Eastern Africa"
countries <- countries %>% left_join(reg_subregs, by = "subregion")

# 5. Towns
towns <- st_read("../inst/extdata/ne_10m_populated_places_simple/ne_10m_populated_places_simple.shp") %>% 
    select(iso2 = iso_a2, iso3 = adm0_a3, name, capital = featurecla, pop = pop_max) %>%
    mutate(capital = ifelse((capital == "Admin-0 capital" &
                             ! name %in% c("Sucre", "Abidjan", "Yangon",
                                              "Bloemfontein", "Cape Town", "Johannesburg")) |
                            name %in% c("Ramallah", "Hong Kong"), TRUE, FALSE),
           iso2 = ifelse(name == "Hargeisa", "SO2", iso2),
           iso2 = ifelse(name == "Pristina", "XK", iso2),
           pop = ifelse(pop == -99, NA, pop)) %>% 
    rename(point = "geometry")
id_AK <- towns[filter(countries, country == "Alaska"), ] %>% pull(name)
id_HI <- towns[filter(countries, country == "Hawaii"), ] %>% pull(name)
id_PT_20 <- towns[filter(countries, country == "Azores"), ] %>% pull(name)
id_PT_30 <- towns[filter(countries, country == "Madeira"), ] %>% pull(name)
id_IC <- towns[filter(countries, country == "Cannaries"), ] %>% pull(name)
towns <- towns %>%
    mutate(iso2 = case_when(name %in% id_AK    & iso2 == "US" ~ "US-AK",
                          name %in% id_HI    & iso2 == "US" ~ "US-HI",
                          name %in% id_PT_20 & iso2 == "PT" ~ "PT-20",
                          name %in% id_PT_30 & iso2 == "PT" ~ "PT-30",
                          name %in% id_IC    & iso2 == "ES" ~ "IC",
                          .default = iso2))
caps <- towns %>% as_tibble %>% filter(capital) %>%
    select(iso2, capital = name, point)
# no capitals for nauru, northern cyprus and macao
countries <- countries %>% as_tibble %>%
    left_join(caps, by = "iso2") %>%
    relocate(capital, .after = sovereign) %>% 
    st_sf(agr = "polygon")
ne_towns <- towns

# 6. Plot of countries and subregions
plot_countries <- FALSE
if (plot_countries){
    cties <- countries %>% filter(country == sovereign)
    for (i in 1:nrow(cties)){
        actie <- slice(cties, i)
        name <- pull(actie, country)
        aplot <- actie %>% ggplot + geom_sf() + geom_sf(data = bb_bbox(st_bbox(actie)), fill = NA) +
            theme_minimal() + ggtitle(name)
        print(aplot)
        Sys.sleep(0.5)
    }
}
plot_subregs <- FALSE
if (plot_subregs){
    for (i in 1:nrow(subregs)){
        asreg <- slice(subregs, i)
        name <- pull(asreg, subregion)
        aplot <- asreg %>% ggplot + geom_sf() + geom_sf(data = bb_bbox(st_bbox(asreg)), fill = NA) +
            theme_minimal() + ggtitle(name)
        print(aplot)
        Sys.sleep(1)
    }
}

ne_countries <- countries %>% st_sf(agr = "polygon")

ne_countries <- ne_countries %>% select(iso2:capital, status,
                                        en:it,
                                        region, subregion, wbregion,
                                        pop, gdp, economy, income,
                                        point)


# Some more informations for parts
Cannaries <- list(2153389,   63741, "Canary Islands",   "Îles Canaries",  "Kanarische Inseln",  "Canarias",        "isole Canarie")
`Svalbard and Jan Mayen` <- list(2939,      1, "Svalbard Jan Mayen",      "Svalbard Jan Mayen",   "Spitzbergen Jan Mayen", "Svalbard Jan Mayen",    "Isole Svalbard Jan Mayen")
Bouvet <- list(0,      0, "Bouvet Island", "île Bouvet", "Bouvetinsel",  "Isla Bouvet",  "isola Bouvet")
Guyane <- list(294071,    1874, "French Guiana", "Guyane",     "Französisch-Guayana", "Guayana Francesa", "Guyana francese")
Martinique <- list(376480,    7391, "Martinique",    "Martinique",  "Martinique",          "Martinica",        "Martinica")
Guadeloupe <- list(395700,    4245, "Guadeloupe",    "Guadeloupe", "Guadeloupe",          "Guadalupe",        "Guadalupa")
Reunion <- list(859959,    5254, "Réunion",       "La Réunion", "Réunion",             "Reunión",          "Riunione")
Mayotte <- list(279471,    1152, "Mayotte",       "Mayotte",    "Mayotte",             "Mayotte",          "Mayotte")
Alaska <- list(710249,    44900, "Alaska",                   "Alaska",     "Alaska",             "Alaska",         "Alaska")
Hawaii <- list(1415872,    47000, "Hawaii",                   "Hawaï",      "Hawaii",             "Hawái",          "Hawaii")
Navassa <- list(0,     0, "Navassa Island", "île de la Navasse",  "Navassa",        "Isla de Navaza",   "isola Navassa")
`United States Minor Outlying Islands` <- list(300,     0,
                                               "United States Minor Outlying Islands",
                                               "Îles mineures éloignées des États-Unis",
                                               "United States Minor Outlying Islands",
                                               "Islas ultramarinas de Estados Unidos",
                                               "Isole minori esterne degli Stati Uniti d'America")
Madeira <- list(289000,   6414, "Madeira",  "Madère",   "Autonome Region Madeira", "Madeira",  "Madera")
Azores <- list(242796,   4492, "Azores",  "Açores",   "Azoren",                  "Azores",   "Azzorre")
`Cocos Islands` <- list(544,     7, "Cocos",            "îles Cocos",    "Kokosinseln",     "islas Cocos",     "Isole Cocos")
`Christmas Island` <- list(1843,    27, "Christmas Island", "île Christmas", "Weihnachtsinsel", "Isla de Navidad", "Isola di Natale")
Annobon <- list(5314,    43, "Annobón", "Province Annobón",             "Provinz Annobón", "Provincia de Annobón", "provincia di Annobón")
`Easter Island` <- list(7750,    115, "Easter Island",      "île de Pâques",    "Osterinsel",    "Isla de Pascua",     "Isola di Pasqua")
Tokelau <- list(1499, 2, "Tokelau", "Tokelau", "Tokelau", "Tokelau", "Tokelau")
Chatham <- list(663,     0, "Chatham Islands", "Îles Chatham", "Chathaminseln", "Islas Chatham", "Isole Chatham")
Kermadec <- list(6,     0, "Kermadec Islands", "Îles Kermadec", "Kermadecinseln",  "Islas Kermadec", "Isole Kermadec")
prince_edwards_islands <- list(25,     0, "Prince Edward Islands",
                               "Archipel du Prince-Édouard",
                               "Prinz-Edward-Inseln",
                               "Islas del Príncipe Eduardo",
                               "Isole del Principe Edoardo")

Bornholm <- list(39439, 2373., "Bornholm", "Bornholm", "Bornholm",  "Bornholm", "Bornholm")
galapagos <- list(25000,   154, "Galápagos Province", "Galápagos",  "Provinz Galápagos",  "Galápagos", "provincia delle Galápagos")
macquarie <- list(30,     1, "Macquarie Island", "île Macquarie", "Macquarieinsel",
                  "Isla Macquarie",  "isola Macquarie")


ne_countries[ne_countries$country == "Cannaries", c("pop", "gdp", "en", "fr", "de", "es", "it")] <- Cannaries
ne_countries[ne_countries$country == "Svalbard and Jan Mayen", c("pop", "gdp", "en", "fr", "de", "es", "it")] <- `Svalbard and Jan Mayen`
ne_countries[ne_countries$country == "Bouvet", c("pop", "gdp", "en", "fr", "de", "es", "it")] <- Bouvet
ne_countries[ne_countries$country == "Guyane", c("pop", "gdp", "en", "fr", "de", "es", "it")] <- Guyane
ne_countries[ne_countries$country == "Martinique", c("pop", "gdp", "en", "fr", "de", "es", "it")] <- Martinique
ne_countries[ne_countries$country == "Guadeloupe", c("pop", "gdp", "en", "fr", "de", "es", "it")] <- Guadeloupe
ne_countries[ne_countries$country == "Reunion", c("pop", "gdp", "en", "fr", "de", "es", "it")] <- Reunion
ne_countries[ne_countries$country == "Mayotte", c("pop", "gdp", "en", "fr", "de", "es", "it")] <- Mayotte
ne_countries[ne_countries$country == "Alaska", c("pop", "gdp", "en", "fr", "de", "es", "it")] <- Alaska
ne_countries[ne_countries$country == "Hawaii", c("pop", "gdp", "en", "fr", "de", "es", "it")] <- Hawaii
ne_countries[ne_countries$country == "Navassa", c("pop", "gdp", "en", "fr", "de", "es", "it")] <- Navassa
ne_countries[ne_countries$country == "United States Minor Outlying Islands", c("pop", "gdp", "en", "fr", "de", "es", "it")] <- `United States Minor Outlying Islands`
ne_countries[ne_countries$country == "Madeira", c("pop", "gdp", "en", "fr", "de", "es", "it")] <- Madeira
ne_countries[ne_countries$country == "Azores", c("pop", "gdp", "en", "fr", "de", "es", "it")] <- Azores
ne_countries[ne_countries$country == "Cocos Islands", c("pop", "gdp", "en", "fr", "de", "es", "it")] <- `Cocos Islands`
ne_countries[ne_countries$country == "Christmas Island", c("pop", "gdp", "en", "fr", "de", "es", "it")] <- `Christmas Island`
ne_countries[ne_countries$country == "Annobon", c("pop", "gdp", "en", "fr", "de", "es", "it")] <- Annobon
ne_countries[ne_countries$country == "Easter Island", c("pop", "gdp", "en", "fr", "de", "es", "it")] <- `Easter Island`
ne_countries[ne_countries$country == "Tokelau", c("pop", "gdp", "en", "fr", "de", "es", "it")] <- Tokelau
ne_countries[ne_countries$country == "Chatham", c("pop", "gdp", "en", "fr", "de", "es", "it")] <- Chatham
ne_countries[ne_countries$country == "Kermadec", c("pop", "gdp", "en", "fr", "de", "es", "it")] <- Kermadec
ne_countries[ne_countries$country == "Prince Edward Islands", c("pop", "gdp", "en", "fr", "de", "es", "it")] <- prince_edwards_islands
ne_countries[ne_countries$country == "Bornholm", c("pop", "gdp", "en", "fr", "de", "es", "it")] <- Bornholm
ne_countries[ne_countries$country == "Galapagos", c("pop", "gdp", "en", "fr", "de", "es", "it")] <- galapagos
ne_countries[ne_countries$country == "Macquarie Island", c("pop", "gdp", "en", "fr", "de", "es", "it")] <- macquarie
ne_countries[ne_countries$country %in% c("Reunion", "Mayotte", "Guyane", "Martinique", "Guadeloupe",
                                         "Bonaire, Sint Eustatius and Saba", "Bouvet",
                                         "Svalbard and Jan Mayen",
                                         "Tokelau", "Christmas Island", "Cocos Islands", "Kosovo"), "iso3"] <-
    c("REU", "MYT", "GUF", "MTQ", "GLP", "BES", "BVT", "SJM", "TKL", "CXR", "CCK", "UNK")

# 7. Misc informations

ne_countries <- ne_countries %>%
    mutate(country = case_when(country == "Republic of the Congo" ~ "Congo",
                               country == "Democratic Republic of the Congo" ~ "D.R. Congo",
                               country == "Republic of Serbia" ~ "Serbia",
                               country == "Hong Kong S.A.R." ~ "Hong Kong",
                               country == "The Bahamas" ~ "Bahamas",
                               country == "Macao S.A.R" ~ "Macao",
                               country == "United Republic of Tanzania" ~ "Tanzania",
                               .default = country)) %>%
    mutate(sovereign = ifelse(type == "main", country, sovereign))

## 7.1. tibble of entities
countries <- ne_countries %>% pull(country) %>% tibble(name = .) %>% add_column(type = "country")
sovereigns <- ne_countries %>% pull(sovereign) %>% unique %>%
    tibble(name = .) %>% add_column(type = "sovereign")
regions <- ne_countries %>% pull(region) %>% unique %>%
    tibble(name = .) %>% add_column(type = "region")
subregions <- ne_countries %>% pull(subregion) %>% unique %>%
    na.omit %>% tibble(name = .) %>% add_column(type = "subregion")
wbregions <- ne_countries %>% pull(wbregion) %>% unique %>%
    na.omit %>% tibble(name = .) %>% add_column(type = "wbregion")
entities <- countries %>% bind_rows(sovereigns) %>%
    bind_rows(regions) %>% bind_rows(subregions) %>%
    bind_rows(wbregions)

simplify <- TRUE
if (simplify){
    #min_area <- units::set_units(3E9, m^2)
    min_area <- units::set_units(3E09, m^2)
    countries_sfc <- ne_countries %>% st_geometry
    z <- countries_sfc %>% st_intersects(countries_sfc)
    # keep_size is the list of the sfg that should be kept as such
    islands <- z %>% sapply(length)
    islands <- islands == 1
    a <- ne_countries %>% st_geometry %>% st_area
    keep_size <- tibble(country = ne_countries$country, area = a, islands = islands) %>%
        filter(islands, area < min_area) %>%
        mutate(area = units::set_units(area, km^2))
    islands_names <- keep_size %>% pull(country)
    ne_countries_keep <- ne_countries %>% filter(country %in% islands_names)
    ne_countries_reduce <- ne_countries %>% filter(! country %in% islands_names)
    ne_countries_reduce_geometry <- ne_countries_reduce %>% st_geometry %>%
        rmapshaper::ms_simplify(keep = 0.1, keep_shapes = TRUE)
    ne_countries_reduce$polygon <- ne_countries_reduce_geometry
    ne_countries <- ne_countries_keep %>% bind_rows(ne_countries_reduce) %>% arrange(country)
} else islands_names <- character(0)

simplify_all <- FALSE
if (simplify_all){
    simp_sfc <- ne_countries %>% st_geometry %>%
        rmapshaper::ms_simplify(keep = 0.1, keep_shapes = TRUE)
    ne_countries$polygon <- simp_sfc
}
for (i in c("en", "de", "it", "es", "fr"))
    ne_countries[[i]] <- stringi::stri_escape_unicode(ne_countries[[i]])


## 7.2 Coastlines 
pb <- c("Mali", "Algeria", "Burkina Faso", "Togo", "Ghana", "France", "Spain", "United Kingdom", "Antarctica")
ok <- ne_countries %>% filter(! country %in% c(pb, islands_names)) %>% st_geometry
ne_lands <- ne_countries %>% filter(! country %in% islands_names) %>% st_geometry %>% st_union
pb_sfc <- ne_countries %>% filter(country %in% pb) %>% st_geometry

cut_east_west <- function(x){
    bb_ew <- x %>% st_bbox
    bb_w <- bb_e <- bb_ew
    bb_w[3] <- -1E-03
    bb_e[1] <- +1E-03
    x_west <- x %>% st_crop(bb_w)
    x_east <- x %>% st_crop(bb_e)
    c(x_west, x_east)
}

oks <- ok %>% st_geometry
pbs <- Reduce("c", lapply(pb, function(i) cut_east_west(st_geometry(filter(ne_countries, country == i)))))
all <- c(oks, pbs)
ne_lands_shift <- all %>% st_shift_longitude %>% st_union


## countries_sfc <- ne_countries %>% st_geometry
## z <- countries_sfc %>% st_intersects(countries_sfc)
## islands <- z %>% sapply(length)
## islands <- islands == 1
## a <- ne_countries %>% st_geometry %>% st_area
## keep_size <- tibble(country = ne_countries$country, area = a, islands = islands) %>%
##     filter(islands, area < units::set_units(1E11, m^2)) %>%
##     pull(country)

## ne_countries_keep <- ne_countries %>% filter(country %in% keep_size)
## ne_countries_reduce <- ne_countries %>% filter(! country %in% keep_size)
## ne_countries_reduce_geometry <- ne_countries_reduce %>% st_geometry %>% rmapshaper::ms_simplify(keep = 0.1, keep_shapes = TRUE)
## ne_countries_reduce$polygon <- ne_countries_reduce_geometry
## ne_countries <- ne_countries_keep %>% bind_rows(ne_countries_reduce) %>% arrange(country)


#options(old_options)
