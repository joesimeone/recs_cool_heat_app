library(tidyverse)
library(tigris)
library(leaflet)
library(mapview)
library(sf)
library(maps)
library(htmltools)


## Join AC data with shapefile ----------------------------------------------
# 
# st_shp <- tigris::states(year = 2020)
# 
# st_tbl <- read_csv(here("data", "st_tbl_fin.csv"))
# st_shp <- st_transform(st_shp, crs = 4326)
# 
# st_ac_shp <- st_shp %>% left_join(st_tbl,
#                                   by = c("NAME" = "states"))

# 
# ## Filter out territories (No data available)
# territories <- filter(st_ac_shp, is.na(estimates_rse)) %>% select(NAME) %>% names()
# 
# st_ac_clean <- filter(st_ac_shp, !NAME %in% territories)
# 
# 
# # install.packages("maps")
# 
# tst_map <- filter(st_ac_clean, cool_type_fac == "Any AC" & stat == "pct")

render_rse_map <- function(dat){

## Set bins
rse_bins <- c(0, 5,10, Inf)

## Color Palette for rse map
rse_pal <- colorBin(MetBrewer::MetPalettes$Isfahan1[[1]],
                    domain = dat$estimates_rse,
                    bins = rse_bins)

## Labels for map pop-up
rse_labels <- sprintf("<strong>%s:</strong><br/>RSE = %g",
                      dat$NAME, dat$estimates_rse) %>%
                      lapply(htmltools::HTML)


rse_map <- leaflet(dat) %>%
           setView(-96, 37.8, 4) %>%
           addTiles() %>%
           addPolygons(
                fillColor = ~rse_pal(estimates_rse),
                  weight = 2,
                  opacity = 3,
                  color = "white",
                  dashArray = "2",
                  fillOpacity = .7,
          highlightOptions =
              highlightOptions(
                  weight = 5,
                  color = "#666",
                  dashArray = "",
                  fillOpacity = 0.7,
                  bringToFront = TRUE),
          label = rse_labels,
          labelOptions =
            labelOptions(style =
                           list("font-weight" = "normal",
                                padding = "3px 8px"),
                                textsize = "12px",
                                direction = "auto"))

return(rse_map)}

# 
render_est_map <- function(dat){
  
## Bin width
est_bins <- seq(0, 100, by = 10)

## Color palette for maps 
est_pal <- colorBin("Blues",
                    domain = dat$estimates,
                    bins = est_bins)

## Labels
est_labels <- sprintf("<strong>%s:</strong><br/>
                        Upper Bound = %g <br/>
                        Estimate = %g <br/>
                        Lower Bound = %g <br/>
                        RSE = %g",
                      dat$NAME,
                      round(dat$conf_int_upper, 2),
                      round(dat$estimates, 2),
                      round(dat$conf_int_lower, 2),
                      round(dat$estimates_rse, 2)) %>%
                      lapply(htmltools::HTML)


est_map <- leaflet(dat) %>%
           setView(-96, 37.8, 4) %>%
           addTiles() %>%
           addPolygons(
              fillColor = ~est_pal(estimates),
               weight = 2,
               opacity = 3,
               color = "white",
               dashArray = "2",
               fillOpacity = .7,
          highlightOptions = 
              highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
          label = est_labels,
          labelOptions =
            labelOptions(style =
                           list("font-weight" = "normal",
                                padding = "3px 8px"),
                         textsize = "12px",
                         direction = "auto"))

return(est_map)}
