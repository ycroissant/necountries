library("foreign")
library("dplyr") 
slave_trade <- haven::read_dta("../inst/extdata/slave_trade_QJE.dta") %>% 
    transmute(country = country,
              region = factor(region_w + 2 * region_e + 3 * region_s +
                              4 * region_n + 5 * region_c,
                              labels =c("west", "east", "south", "north", "central")),
              disteq = abs_latitude,
              longitude = longitude,
              area  = land_area * 1E03,
              pop    = round(exp(ln_export_area - ln_export_pop) * area * 1E03, 3),
              coastline = exp(ln_coastline_area) * area,
              island = factor(island_dum, levels = c(0, 1), labels = c("no", "yes")),
              islam = islam / 100,
              colony = factor(colony0 * 1 + colony1 * 2 + colony2 * 3 + colony3 * 4 +
                              colony4 * 5 + colony5 * 6 + colony6 * 7 + colony7 * 8,
                              labels = c('none', 'uk', 'france', 'portugal', 'belgium',
                                         'spain', 'germany', 'italy')),
              legor =  factor(legor_fr + 2 * legor_uk, labels = c("french", "british")),
              gdp   = round(exp(ln_maddison_pcgdp2000)),
              slaves = round(exp(ln_export_area) * area / 1E03, 3),
              slavesarea = exp(ln_export_area),
              humidmax = humid_max,
              rainmin = rain_min,
              lowtemp = low_temp,
              gold = exp(ln_avg_gold_pop),
              oil = exp(ln_avg_oil_pop),
              diamond = exp(ln_avg_all_diamonds_pop),
              atlantic = atlantic_distance_minimum,
              indian = indian_distance_minimum,
              redsea = red_sea_distance_minimum,
              sahara = saharan_distance_minimum) %>%
    mutate(oil = ifelse(oil < 1E-04, 0, oil),
           diamond = ifelse(diamond < 1E-03, 0, diamond),
           gold = ifelse(gold < 1E-06, 0, gold),
           coastline = ifelse(coastline < 15, 0, coastline),
           legor = relevel(legor, "british"))

