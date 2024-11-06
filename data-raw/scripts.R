source("read_naturalearth.R")
source("slave_trade.R")
source("sp_solow.R")
usethis::use_data(ne_countries, ne_towns, sp_solow, slave_trade,
                  compress = 'xz', overwrite = TRUE)
usethis::use_data(entities, ne_lands, ne_lands_shift, countries_list,
                  sovereignty_list, Spain, compress = 'xz',
                  overwrite = TRUE, internal = TRUE)
