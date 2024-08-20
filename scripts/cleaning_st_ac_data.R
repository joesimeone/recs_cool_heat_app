library(here)
library(tidyverse)
library(readxl)
library(janitor)


recs_therm_info <- list(

recs_therm = read_excel(here("data", "recs_st_ac_2020.xlsx"),
                         sheet = "data", skip = 4) %>% clean_names(),

recs_rse  = read_excel(here("data", "recs_st_ac_2020.xlsx"),
                         sheet = "rse", skip = 4) %>% clean_names()
)

## Clean up names / remove blank rows

## Can't think of a good pattern to name each column w/ so just doing by hand :(
recs_therm_info<- map(recs_therm_info, 
                      ~.x %>% 
                      rename(states = x1,
                             all_cool_tot_per_mil = x2,
                             ac_tot_per_mil = uses_air_conditioning_equipment,
                             any_ac_pct = x4,
                             central_ac_tot_per_mil = uses_central_air_conditioning_unitb,
                             central_ac_pct = x6,
                             wall_window_ac_tot_per_mil = uses_individual_air_conditioning_unitc,
                             wall_window_ac_pct = x8,
                             dehumidifier_tot_per_mil = x9,
                             dehumidifier_pct = x10,
                             ceiling_fan_total_per_mil = x11,
                             ceiling_fan_pct = x12) %>% na.omit()
)




## Individual states won't sum to totals so we're going to clean this up separately

## There are Qs to denote surpressed data in some of the fields - warning that they yield NA
total_tbls <- map(recs_therm_info, 
                 ~.x %>% 
                 filter(states == "All homes") %>%
                 mutate(across(-states, 
                               ~ if (is.character(.)) parse_number(.) 
                               else .)) %>% 
                 pivot_longer(cols = !states,
                                      names_to = "cool_type",
                                      values_to = "estimates") %>%
                 mutate(stat = if_else(str_detect(cool_type, "_pct"), 
                                       "pct", "count_per_mill")) %>% 
                 mutate(states = snakecase::to_snake_case(states))
)
                 

total_tbls <- map(total_tbls, 
                 ~.x %>% 
                  mutate(cool_type = gsub("_tot_per_mil", "", 
                                          .x[["cool_type"]]),
                         cool_type = gsub("_pct", "", 
                                          .x[["cool_type"]])
                         )
)


total_tbls$recs_rse <- total_tbls$recs_rse %>% 
                      mutate(estimates_rse = estimates) %>% 
                      select(-estimates)



total_tbl_fin <- total_tbls$recs_therm %>% 
                 left_join(total_tbls$recs_rse, 
                           by = c("states", "cool_type", "stat"))


## There are Qs to denote surpressed data in some of the fields - warning that they yield NA
st_tbls <- map(recs_therm_info, 
              ~.x %>% 
              filter(states != "All homes") %>% 
              mutate(across(-states, 
                            ~ if (is.character(.)) parse_number(.) 
                              else .)) %>% 
              pivot_longer(cols = !states,
                           names_to = "cool_type",
                           values_to = "estimates") %>%
              mutate(stat = if_else(str_detect(cool_type, "_pct"), 
                                    "pct", "count_per_million"))
              
              
)


st_tbls <- map(st_tbls, 
                 ~.x %>% 
                   mutate(cool_type = gsub("_tot_per_mil", "", 
                                           .x[["cool_type"]]),
                          cool_type = gsub("_pct", "", 
                                           .x[["cool_type"]])
                   )
)


st_tbls$recs_rse <- st_tbls$recs_rse %>% 
                    mutate(estimates_rse = estimates) %>% 
                    select(-estimates)

st_tbl_fin <- st_tbls$recs_therm %>% 
                 left_join(st_tbls$recs_rse, 
                            by = c("states", "cool_type", "stat"))


total_tbl_fin <- total_tbl_fin %>%
                 mutate(std_error = (estimates_rse / 100)*estimates,
                        conf_int_upper = 1.96*std_error + estimates,
                        conf_int_lower = estimates - 1.96*std_error)

st_tbl_fin <- st_tbl_fin %>%
                 mutate(std_error = (estimates_rse / 100)*estimates,
                        conf_int_upper = 1.96*std_error + estimates,
                        conf_int_lower = estimates - 1.96*std_error)


write_csv(total_tbl_fin, here("data", "total_tbl_fin.csv"))
write_csv(st_tbl_fin, here("data", "st_tbl_fin.csv"))



st_tbl_fin %>% filter(stat == "pct"  & states == "Delaware") %>% ggplot(aes(x=cool_type, y=estimates)) +         
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = conf_int_lower, 
                    ymax = conf_int_upper), 
                    linetype = "dashed", width = 0) + coord_flip()



