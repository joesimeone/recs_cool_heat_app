
library(tidyverse)
library(here)


# Imports -----------------------------------------------------------------
st_tbl <- read_csv(here("data", "st_tbl_fin.csv"))

st_shp <- tigris::states(year = 2020)
st_shp <- st_transform(st_shp, crs = 4326)




# Last bit of cleaning  ---------------------------------------------------
st_tbl <- st_tbl %>% 
  filter(cool_type != "all_cool_tot_per_mil") %>% 
  mutate(cool_type_fac = 
           case_when(
             str_detect(cool_type, "wall_window") ~ "Wall or Window Unit",
             str_detect(cool_type, "dehumidifier") ~ "Dehumidifer",
             str_detect(cool_type, "central_ac") ~ "Central Air",
             str_detect(cool_type, "ac_tot") | 
             str_detect(cool_type, "any_ac") ~ "Any AC",
             str_detect(cool_type, "ceiling_fan") ~ "Uses Ceiling Fan"),
         stat = 
           case_when(stat == "pct" ~ "Percentage",
                     stat == "count_per_million" ~ "Counts Per Million"
           )
         )



# Joining data w/ shapefile for maps  -------------------------------------
st_tbl_shp <- st_shp %>% 
              left_join(st_tbl,
                        by = c("NAME" = "states"))

## Filter out territories (No data available)
territories <- filter(st_tbl_shp, is.na(estimates_rse)) %>% 
               select(NAME) %>% names()

st_tbl_shp <- filter(st_tbl_shp, !NAME %in% territories)