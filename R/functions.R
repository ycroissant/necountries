#library(sf);library(tidyverse);sf_use_s2(FALSE);load("./R/sysdata.rda");library(classInt);load("./data/ne_countries.rda");load("./data/ne_towns.rda");load("./data/slave_trade.rda");load("./data/sp_solow.rda");library(ggrepel);load("./R/sysdata.rda")

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%` 


#' @importFrom dplyr left_join
#' @export
dplyr::left_join

#' @importFrom dplyr select
#' @export
dplyr::select


extend_bbox <- function(x, scale = 2){
    if (length(scale) == 2){
        .xscale <- scale[1]
        .yscale <- scale[2]
    }
    else .xscale <- .yscale <- scale
    .bb <- st_bbox(x)
    .xmin <- .bb[1]
    .xmax <- .bb[3]
    .ymin <- .bb[2]
    .ymax <- .bb[4]
    .xrange <- .xmax - .xmin
    .yrange <- .ymax - .ymin
    .xcenter <- (.xmax + .xmin) / 2
    .ycenter <- (.ymax + .ymin)  /2
    .xmin <- .xcenter - .xrange * .xscale / 2
    .xmax <- .xcenter + .xrange * .xscale / 2
    .ymin <- .ycenter - .yrange * .yscale / 2
    .ymax <- .ycenter + .yrange * .yscale / 2
    bb(.xmin, .xmax, .ymin, .ymax, crs = st_crs(x))
}


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

bb_bbox_N <- function(x, N = 10){
    .crs <- st_crs(x)
    .bb <- st_bbox(x)
    .xmin <- .bb[1]
    .xmax <- .bb[3]
    .ymin <- .bb[2]
    .ymax <- .bb[4]
    xs <- .xmin + (0:(N - 1)) * (.xmax - .xmin) / (N - 1)
    ys <- .ymin + (0:(N - 1)) * (.ymax - .ymin) / (N - 1)
    x_col <- c(xs,            rev(xs),       .xmin)
    y_col <- c(rep(.ymin, N), rep(.ymax, N), .ymin)
    x_col <- c(xs, rep(.xmax, N), rev(xs), rep(.xmin, N))
    y_col <- c(rep(.ymin, N), ys, rep(.ymax, N), rev(ys))
    .mat <- matrix(c(x_col, y_col), ncol = 2)
    st_polygon(list(.mat)) %>% st_sfc(crs = .crs)
}

#' Universal Transverse Mercator projection
#'
#' `utm` returns the relevant UTM crs (in the 'proj4string' form)
#'
#' There is one utm projections for each of the 60 zones that divide
#' the world. The zone can be indicated as an integer (ie 12L, and not
#' 12) or can be computed from a `sf` object
#'
#' @name utm
#' @param x either an integer (from 1L to 60L) or a `sf
#' @return a character string
#' @importFrom sf st_bbox
#' @importFrom rlang .data
#' @return a character (a crs i, the 'proj4string' format)
#' @export
#' @examples
#' we <- countries("Western Europe")
#' utm(we)
#' utm(32L)
utm <- function(x){
    if (inherits(x, "sf")){
        lims <- st_bbox(x)[c(1, 3)]
        if (round(mean(abs(lims))) > 170) zone <- 1
        else zone <- round((mean(lims) + 3) / 6 + 30)
    }
    else{
        if(! is.integer(x)) stop("x should be an integer, ie use 12L instead of 12")
        zone <- x
    }
    paste("+proj=utm +zone=", zone,
          " +datum=WGS84 +units=m +no_defs +type=crs",
          sep = "")
}

#' Populated places from naturalearth 
#'
#' Select a set of cities; the set can be defined using the id of the
#' country, the fact that it is a capital and the size
#'
#' @name towns
#' @param x a `sf` (typically computed using the `countries`
#'     function), or a character that is passed to `countries`,
#' @param size the minimum size of the cities that have to be
#'     retrieved (the default value is `NULL` and all the cities are
#'     retrieved)
#' @param capital if `TRUE` always retrieve the capitals, even if
#'     their size is below the one specified using the `size` argument
#' @param crs an optional **crs** which is passed to `st_transform`
#' @param shift a boolean, if `TRUE`, `st_shift_longitude` is used
#' @return a `sf` containing five columns:
#' - `iso2`: the id of the  country,
#' - `iso3`: the id of the  country,
#' - `name`: the name of the city,
#' - `capital`: a boolean, `TRUE` for a capital
#' - `pop`: the population of the city,
#' - `point`: a point `sfc` containing the coordinates of the city
#' @importFrom dplyr filter pull `%>%`
#' @importFrom sf st_transform st_shift_longitude
#' @export
#' @examples
#' we <- countries("Western Europe")
#' towns(we, size = 1E06, capital = TRUE)
towns <- function(x, size = NULL, capital = FALSE, crs = NULL, shift = FALSE){
    .towns <- necountries::ne_towns
    if (! inherits(x, "sf")){
        x <- countries(x)
    }
    x <- x %>% pull("iso2")
    twns <- .towns %>% filter(.data$iso2 %in% x)
    if (is.null(size)) size <- 0
    if (capital) twns <- twns %>% filter(.data$pop >= size | .data$capital)
    else twns <- twns %>% filter(.data$pop >= size)
    if (shift) twns <- twns %>% st_shift_longitude
    if (! is.null(crs) && ! is.logical(crs)) twns <- twns %>% st_transform(crs)
    twns
}

#' Countries from naturalearth
#'
#' Select a set of countries; talking about countries, we mean either
#' sovereign countries, parts of countries and dependencies, each of
#' these cathegories being on each own row. A single or a set of
#' countries can be obtained by indicating a set of names of, either
#' countries, regions or subregions
#'
#' @name countries
#' @param name a character vector that contains one or several
#'     countries, regions or subregions (mixing the two of the three
#'     cathegories will result as an error),
#' @param part should the parts of the countries be included (eg
#'     Azsores for Portugal or Alaska for the United States of
#'     America),
#' @param dependency should the dependencies of the countries be
#'     included (eg Greenland and the Faroe Islands for Denmark),
#' @param indeterminate should the indeterminate territories be
#'     included,
#' @param exclude an optional set of countries that should be excluded
#'     from the request,
#' @param include an optional set of countries that should be included
#' @param utm if `TRUE`, the geometry is transformed using the
#'     relevant utm projection, if an integer, the geometry is
#'     transformed using the utm projection for the zone indicated,
#' @param crs a CRS
#' @param towns if `TRUE`, a tibble containing the cities of the
#'     countries selected is returned as a `"towns"` attribute,
#' @param capital if `TRUE` the tibble containing the cities of the
#'     countries selected will contain the capitals, whatever their
#'     size,
#' @param lang the language for countries and towns, one of `"en"`,
#'     `"fr"`, `"es"`, `"it"
#' @param extend a number >= 1, extend the bounding box so that the
#'     background is larger than the initial bounding box and can be
#'     transformed correctly if utm transformation is required
#' @param shift a boolean, if `TRUE`, `st_shift_longitude` is used
#' @param coastlines a boolean, `TRUE` to get the background coastines
#' @return an object of class `countries` which inherits from `sf`
#'     with the following columns:
#' - `id` the two letters identifier of the country,
#' - `type` either `"main"` (the main part of a sovereign country, the
#'     whole country for most of them) - `country` the name of the
#'     entity,
#' - `sovereign` the sovereign country the entity belongs to,
#' - `capital` the name of the capital of the country (NA for parts
#'     and dependencies) - `subregion` the name of the subregion
#'     (United Nations' definition)
#' - `pop` the population of the entity,
#' - `gdp` currently undocumented
#' - `wbreg` the name of the region (World Bank's definition)
#' - `region` the name of the region (United Nations' definition) Two
#'     attributes `"type"` and `"towns"`
#' @importFrom dplyr pull filter mutate distinct
#' @importFrom sf st_transform st_polygon st_sfc st_crs st_crop
#'     st_intersection sf_use_s2
#' @importFrom classInt classIntervals
#' @importFrom stringi stri_unescape_unicode
#' @export
#' @examples
#' countries("Western Europe")
countries <- function(name = NA,
                      part = FALSE,
                      dependency = FALSE,
                      indeterminate = FALSE,
                      exclude = NULL,
                      include = NULL, 
                      utm = FALSE,
                      crs = NULL, 
                      towns = FALSE,
                      capital = FALSE,
                      lang = NULL,
                      extend = 1,
                      shift = FALSE,
                      coastlines = TRUE){
    if (is.numeric(towns)){
        .size <- towns
        towns <- TRUE
    } else {
        if (towns) .size <- 0
    }
    sf_use_s2(FALSE)
    .extend <- extend
    .lang <- lang
    if (! is.null(.lang)){
        if (! .lang %in% c("en", "fr", "it", "es")) stop("unknown language")
    }
    ne_lands <- ne_lands %>% st_geometry
    .countries <- necountries::ne_countries
    .name <- name
    .utm <- utm
    .crs <- crs
    .shift <- shift
    .coastlines <- coastlines
    
    # name is NA; all the countries are selected
    if (length(name) == 1 && is.na(name)){
        cty <- .countries
        if (! part) cty <- cty %>% filter(.data$type != "part")
        if (! dependency) cty <- cty %>% filter(.data$type != "dependency")
        .types <- "world"
        .bb <- cty %>% bb_bbox_N(1000)
        .bg <- ne_lands %>% st_crop(st_bbox(cty))
    } else {
        # shortcuts for Yougoslavia and USSR
        if ("USSR" %in% .name)
            .name <- setdiff(.name, "USSR") %>%
                c("Kazakhstan", "Tajikistan", "Ukraine", "Belarus",
                  "Lithuania", "Turkmenistan", "Uzbekistan", "Estonia", "Latvia",
                  "Moldova", "Armenia", "Georgia", "Russia")
        if ("Yougoslavia" %in% .name)
            .name <- setdiff(.name, "Yougoslavia") %>%
                c("Kosovo", "Montenegro", "Slovenia", "Croatia",
                  "Bosnia and Herzegovina", "North Macedonia", "Serbia")

#        .countries$country <- .countries[[lang]]
        
        # check for unknown names
        ukn <- setdiff(.name, pull(entities, name))
        if (length(ukn)) stop(paste(paste(ukn, collapse = ", "), " unknown\n", sep = ""))
        # build a tibble with the types of the entities selected and
        # check for consistency
        x <- entities %>% filter(.data$name %in% .name)
        .types <- x %>% pull("type") %>% unique
        if (all(.types %in% c("sovereign", "country"))){
            sovereigns <- x %>% filter(.data$type == "sovereign") %>% pull("name")
            non_sovereigns <- setdiff(.name, sovereigns)
            if (length(sovereigns)){
                sov <- .countries %>% filter(.data$sovereign %in% .name)
                if (! part) sov <- sov %>% filter(! .data$type == "part")
                if (! dependency) sov <- sov %>% filter(! .data$type == "dependency")
            }
            if (length(non_sovereigns)) nsov <- .countries %>% filter(.data$country %in% non_sovereigns)
            if (length(sovereigns) & length(non_sovereigns)) cty <- sov %>% bind_rows(nsov) %>% distinct
            else if (length(sovereigns)) cty <- sov else cty <- nsov
        } else {
            if (length(.types) > 1)
                stop("different kind of entities are selected")
            cty <- .countries %>% filter(.data[[.types]] %in% .name)
            if (! part) cty <- cty %>% filter(.data$type != "part")
            if (! dependency) cty <- cty %>% filter(.data$type != "dependency")
        }
    }
    # select the countries, eventually with including/excluding some
    if (! indeterminate) cty <- cty %>% filter(.data$type != "indeterminate")
    if (! is.null(include)){
        if (! all(include %in% pull(filter(entities, .data$type == "country"), name)))
            stop("unknown country in include")
        cty <- cty %>% bind_rows(filter(.countries, .data$country %in% include))
    }
    if (! is.null(exclude)) cty <- cty %>% filter(! .data$country %in% exclude)
        
    # Bounding box, with a lot of points in order to be abble to
    # reproject it correctly
    # 1.05 marche pas alors que 1.1 marche ?????
    utm_projection <- (! is.logical(.utm) | (is.logical(.utm) & .utm))
    crs_projection <- ! is.null(.crs)

    if (.shift){
        if (.coastlines) coasts <- ne_lands_shift
        cty <- cty %>% st_shift_longitude
    }
    else if (.coastlines) coasts <- ne_lands
    .bb <- cty %>% bb_bbox_N(1000)
        
    if (utm_projection){
        if (is.logical(.utm)) .crs <- utm(cty)
        else .crs <- utm(.utm)
    }
    
    if (utm_projection | crs_projection){
        if (.coastlines) .bg <- st_crop(coasts, extend_bbox(cty, .extend))
        .bb <- .bb %>% st_transform(.crs)
        cty <- cty %>% st_transform(.crs)
        cty <- cty %>% mutate(point = st_transform(.data$point, .crs))
        if (.coastlines) .bg <- .bg %>% st_transform(.crs) %>% st_intersection(.bb)
    }
    else if (.coastlines) .bg <- coasts %>% st_crop(st_bbox(cty))
    if (towns | capital){
        if (! towns) .size <- Inf
        twns <- towns(cty, size = .size, capital = capital, crs = .crs, shift = .shift)
        attr(cty, "towns") <- twns
    }
    if (! is.null(.lang)) cty$country <- stri_unescape_unicode(cty[[.lang]])
    cty <- cty %>% select(- c("en", "fr", "de", "es", "it"))
    class(cty) <- c("countries", class(cty))
    attr(cty, "type") <- .types
    attr(cty, "bg") <- if (coastlines) .bg else NULL
    attr(cty, "bb") <- .bb
    cty
}

#' Compute a unique `sf` to optimize the position of labels
#'
#' Displaying labels on a map is complicated because of serious
#' overlapping problems. Labels for different kinds of entities can be
#' put in a unique `sf`
#'
#' @name labels.countries
#' @param object a `countries` object`,
#' @param \dots further arguments (currently unused),
#' @param var a character vector indicating the entities that should
#'     be labeleld, it can be `country`, `capital` and `towns`
#' @return a `sf` containing:
#' - `name` the names of the entities,
#' - `type` the type of the entity (either `country`, capital` and
#' `towns`)
#' - `point` the coordinate of the points (obtained using
#' `st_point_on_surface` for countries)
#' @importFrom tibble as_tibble add_column
#' @importFrom dplyr select bind_rows
#' @importFrom sf st_geometry st_point_on_surface st_sf
#' @importFrom stats na.omit
#' @export
labels.countries <- function(object, ..., var){
    x <- object
    .label <- var
    .labels <- list()
    if ("country" %in% .label){
        point <- x %>% st_geometry %>% st_point_on_surface
        .labels$country <- x %>% as_tibble %>%
            select(name = "country") %>%
            add_column(point = point) %>%
            add_column(type = "country", .before = 1)
    }
    if ("capital" %in% .label){
        .labels$capital <- x %>% as_tibble %>%
            select(name = "capital", "point") %>%
            add_column(type = "capital", .before = 1) %>% na.omit
    }
    if ("towns" %in% .label){
        if ("capital" %in% .label)
            .labels$town <- attr(x, "towns") %>%
                filter(! .data$capital) %>% select("name") %>%
                add_column(type = "town", .before = 1)
        else .labels$town <- attr(x, "towns") %>% select("name") %>%
                 add_column(type = "town", .before = 1)
        }
    .labels <- .labels %>% Reduce(f = "bind_rows") %>% st_sf(agr = "point")
    .labels
}

#' Basic plot function for `countries` objects
#'
#' As the `plot` method of `sf`, this function is intended to obtain
#' quickly a map for a set of countries. Countries' boundaries are
#' represented and filling can be used, some cities can also be
#' represented and labels can be added. `ggplot` is used and should be
#' used directly when more enhanced maps are required
#'
#' @name plot.countries
#' @param x a `countries` object,
#' @param \dots further arguments (currently unused)
#' @param labels a character vector containing the variables that
#'     should be labeled: `country`, `capital` and/or `towns`
#' @param fill a variable use to fill countries' polygons
#' @param capital,centroid,towns a variable associated with the shape
#'     or the size of points; for towns, the only available variable
#'     is "pop", it town is a boolean fixed points are used with a
#'     specific shape for capitals if shape is of length 2
#' @param bks an optional vector of breaks in order to use a
#'     continuous variable for fill
#' @param n the number of class (passed to `classIntervals`)
#' @param size either an integer for a fixed size or a couple of
#'     integer to define the range of the sizes of the points
#' @param shape either one integer for a fixed shape or a couple of
#'     integer to get a specific shape for capitals
#' @param style the style (passed to `classIntervals`)
#' @param palette the palette (selected in `scale_fill_brewer`)
#' @param bw a boolean, if `TRUE`, a black and white map is produced
#' @param oceans,background a character indicating the color for the
#'     oceans and the background
#' @return a `gg` object.
#' @importFrom sf st_set_geometry
#' @importFrom ggplot2 ggplot aes geom_sf scale_fill_brewer guides
#'     scale_shape_manual scale_size labs
#' @importFrom ggrepel geom_label_repel
#' @examples
#' we <- countries("Western Europe")
#' plot(we)
#' @export
plot.countries <- function(x,
                           ...,
                           labels = NULL,
                           fill = NULL,
                           capital = NULL,
                           centroid = NULL,
                           towns = NULL,
                           bks = NULL,
                           n = 6,
                           size = c(1, 10),
                           shape = c(15, 16),
                           style = NULL,
                           palette = NULL,
                           bw = FALSE,
                           oceans = "lightblue",
                           background = "lightgrey"){
    
    # if bw is TRUE, a black and white map is drawn with white seas
    # and a Greys palette
    # oceans defines the color of the oceans, by default lightblue
    
    if (bw & is.null(palette)) palette <- "Greys"
    .oceans <- oceans
    .background <- background
    .bg <- attr(x, "bg")
    .bb <- attr(x, "bb")
    .type <- attr(x, "type")
    .labels <- labels
    .fill <- fill
    .capital <- capital
    .centroid <- centroid
    .plot_towns <- towns
    .shape <- shape
    .size <- size

    # check the value of .plot_towns, either a logical or "pop"
    if (! is.null(.plot_towns)){
        if (is.character(.plot_towns)){
            if (.plot_towns != "pop"){
                stop("currently only pop can be mapped to the size of the points representing the towns")
            }
        }
        else{
            if (! is.logical(.plot_towns)){
                stop("towns should either be a logical or 'pop'")
            }
            else{
                if (! .plot_towns) .plot_towns <- NULL
            }
        }
    }

    # .plot_towns is now either NULL, "pop" or TRUE
    # size is a numeric of length 1 only if towns are plotted only if
    # .plot_towns = TRUE, otherwise it should be of length 2
    if (length(.size) > 2) stop("size should be of length 1 or 2")
    if ((! is.null(.plot_towns) && is.logical(.plot_towns))){
        if (length(.size) > 1) .size <- .size[1]
    }
    if ((! is.null(.plot_towns) && is.character(.plot_towns))){
        if (length(.size) == 1)
            stop("size should be of length 2")
    }
                
    # .point is the variable that maps either the capital or the centroid
    if (! is.null(.capital) & ! is.null(.centroid))
        stop("only one of capital and centroid should be set")
    if (is.null(.capital) & is.null(.centroid)) .point <- NULL
    if (! is.null(.capital))  .point <- .capital
    if (! is.null(.centroid)){
        .point <- .centroid
        x$point <- x %>% st_geometry %>% st_point_on_surface
    }

    if (! is.null(.plot_towns) & ! is.null(.point))
        stop("non-capital towns can't be plotted if capitals or centroids are mapped with a variable")

    
    .palette <- palette
    .bks <- bks
    .n <- n
    .style <- style
    qual_palettes <- c("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3")
    seq_palettes <- c("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges",
                      "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn",
                      "YlGnBu", "YlOrBr", "YlOrRd")
    div_palettes <- c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu",
                      "RdYlGn", "Spectral")
    if (is.null(.style)) .style <- "pretty"
    if (! is.null(.fill) && ! .fill %in% names(x)) stop("fill unknown")    
    if (is.null(.labels)) .labels <- "none"
    if (length(setdiff(.labels, c("none", "country", "towns", "capital"))) > 0)
        stop("irrelevant value for labels")

    # create the plot with relevant fill
    fill_oceans <- ifelse(bw, "white", .oceans)
    aplot <- .bb %>%
        ggplot +
        geom_sf(fill = fill_oceans)
    if (! is.null(.bg)) aplot <- aplot + geom_sf(data = .bg, fill = .background)

    if (! is.null(.fill)){
        if (! .fill %in% names(x)) stop("unkown variable for fill")
        # fill: qualitative values
        if (is.factor(x[[.fill]]) | is.character(x[[.fill]])){
            if (is.null(.palette)) .palette <- "Set2"
            else if (! .palette %in% qual_palettes) stop("a qualitative palette should be used")
            aplot <- aplot + geom_sf(data = x, aes(fill = .data[[.fill]])) +
                scale_fill_brewer(palette = .palette, na.translate = FALSE) #+ guides(fill = FALSE)
        }
        else {
            # fill: quantitative values
            if (is.null(.bks)){
                .bks <- classIntervals(x[[.fill]], .n, style = .style)$brks
            }
            x[[.fill]] <- cut(x[[.fill]], breaks = .bks)
            if (is.null(.palette)) .palette <- "Blues"
            else if (! .palette %in% c(seq_palettes, div_palettes))
                stop("a sequential or divergent palette should be used")
            aplot <- aplot + geom_sf(data = x, aes(fill = .data[[.fill]])) +
                scale_fill_brewer(palette = .palette, na.translate = FALSE, na.value = "red")
                #+ guides(fill = FALSE)
        }
    }
    else aplot <- aplot + geom_sf(data = x)

    # Towns are plot if there are present and if there is no mapping
    # with capitals or centroid
    # plot_towns <- ! is.null(attr(x, "towns")) now towns plotted only if capital/centroid = NULL
    plot_towns <- ! is.null(attr(x, "towns")) & is.null(.point)
    if (plot_towns) .towns <- attr(x, "towns")

    # If a variable is mapped to capitals or centroids, use different
    # sizes (numerical) or shapes (categorical) variables
    if (! is.null(.point)){
        point_is_numeric <- is.numeric(x[[.point]])
        if (point_is_numeric){
            aplot <- aplot +
                geom_sf(data = st_set_geometry(x, "point"),
                        shape = .shape[1], aes(size = .data[[.point]]))
        }
        else{
            aplot <- aplot +
                geom_sf(data = st_set_geometry(x, "point"),
                        size = .size[1], aes(shape = .data[[.point]]))
        }
        ## if (plot_towns){
        ##     .towns <- filter(.towns, ! capital)
        ##     aplot <- aplot + geom_sf(data = .towns, shape = 15)
        ## }
    }
    if (! is.null(.plot_towns)){
        .towns_data <- attr(x, "towns")
        if (is.character(.plot_towns)){
            if (length(.shape) == 2){
                aplot <- aplot +
                    geom_sf(data = .towns_data,
                            aes(shape = capital, size = .data$pop)) + 
                    scale_shape_manual(values = .shape) +
                    scale_size(range = .size) + 
                    guides(shape = "none") +
                    labs(size = "population")
            }
            else{
                aplot <- aplot +
                    geom_sf(data = .towns_data, shape = .shape,
                            aes(size = .data$pop)) + 
                    scale_size(range = .size) + 
                    labs(size = "population")
            }
        }
        else{
            if (length(.shape) == 2){
                aplot <- aplot +
                    geom_sf(data = .towns_data, size = .size,
                            aes(shape = capital)) + 
                    scale_shape_manual(values = .shape) + 
                    guides(shape = "none")
            }
            else{
                aplot <- aplot +
                    geom_sf(data = .towns_data, size = .size, shape = .shape)
            }
        }
    }

    if (.labels[1] != "none"){
        .labels <- labels(x, var = .labels)
        aplot <- aplot + 
            geom_label_repel(data = .labels,
                             aes(label = .data$name,
                                 geometry = .data$point,
                                 colour = .data$type), fill = "black",
                             stat = "sf_coordinates", max.overlaps = 100) +
            guides(color = "none")
    }
    aplot
}

#' Methods for `dplyr`'s verbs
#'
#' `countries`' objects inherits from `sf`, when a verb of `dlpyr` is
#' used, the returned object is of class `sf`; these methods return a
#' `countries` object.
#'
#' @name dplyr.methods
#' @param x,y,by,copy,suffix,keep see `dplyr::left_join`
#' @param .data see `dplyr::select`
#' @param side for the `check_join` function
#' @param \dots further arguments
#' @return for the `select` and the `left_join` method, a data frame
#' @importFrom dplyr left_join select
#' @importFrom stringr str_wrap
NULL


#' @rdname dplyr.methods
#' @export
check_join <- function(x, y, by = NULL, side = c("right", "both", "left")){
    .side <- match.arg(side)
    .x <- x
    .by <- by
    if (is.null(.by)) .by <- names(y)[1]
    else if (is.numeric(.by)) .by <- names(y)[.by]
    .code <- y[[.by]]
    if (! is.character(.code) & ! is.factor(.code)) stop("The jointing variable should be a character or a factor")
    if (is.factor(.code)) y[[.by]] <- .code <- as.character(.code)
    if (all(nchar(.code) == 2) | all(nchar(.code) == 3)){
        if (all(nchar(.code) == 2)){
            .x_join <- "iso2"
            message("Joining by iso2")
        }
        if (all(nchar(.code) == 3)){
            .x_join <- "iso3"
            message("Joining by iso3")
        }
    }
    else{
        .x_join <- "country"
        message("Joining by countries' names")
    }
    if (.side != "left") unk_countries_y <- setdiff(y[[.by]], .x[[.x_join]])
    if (.side != "right")  unk_countries_x <- setdiff(.x[[.x_join]], y[[.by]])
    if (.side != "left"){
        if (length(unk_countries_y)){
            unk_countries_y <- paste(unk_countries_y, collapse = ", ")
            cat("\nCountries in the external tibble not in the countries' sf:\n",
                str_wrap(unk_countries_y), "\n")
        }
        else cat("\nAll countries of the tibble present in the countries' object\n")
    }
    if (.side != "right"){
        if (length(unk_countries_x)){
            unk_countries_x <- paste(unk_countries_x, collapse = ", ")
            cat("\nCountries in the countries' sf not in the external tibble:\n",
                str_wrap(unk_countries_x), "\n")
        }
        else cat("\nAll countries of the countries' object present in the tibble\n")
    } 
}

#' @rdname dplyr.methods
#' @importFrom dplyr select
#' @export
select.countries <- function(.data, ...){
    cls <- class(.data)
    class(.data) <- setdiff(cls, "countries")
    .data <- .data %>% select(...)
    class(.data) <- cls
    .data
}

#' @rdname dplyr.methods
#' @importFrom dplyr left_join
#' @export
left_join.countries <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ..., keep = NULL){
    .x <- x
    .by <- by
    if (is.null(.by)) .by <- names(y)[1]
    else if (is.numeric(.by)) .by <- names(y)[.by]
    .code <- y[[.by]]
    if (! is.character(.code) & ! is.factor(.code)) stop("The jointing variable should be a character or a factor")
    if (is.factor(.code)) y[[.by]] <- .code <- as.character(.code)
    if (all(nchar(.code) == 2) | all(nchar(.code) == 3)){
        if (all(nchar(.code) == 2)){
            .x_join <- "iso2"
            message("Joining by iso2")
        }
        if (all(nchar(.code) == 3)){
            .x_join <- "iso3"
            message("Joining by iso3")
        }
    }
    else{
        .x_join <- "country"
        message("Joining by countries' names")
    }
    class(.x) <- setdiff(class(.x), "countries")
    .join <- .by
    names(.join) <- .x_join
    .x <- .x %>% left_join(y, by = .join, copy = FALSE, suffix = c(".x", ".y"), ..., keep = NULL)
    class(.x) <- c("countries", class(.x))
    .x
}
