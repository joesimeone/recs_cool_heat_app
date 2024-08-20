library("tidyverse")
library(here)


dropdowns <- function(id, vals){
  
dropdown_search <- selectizeInput(inputId = id, 
                   label = c("Pick your state:" = ""),
                   choices = unique(vals))
    
    return(dropdown_search)
}



measure <- radioButtons(inputId = "measure_type",
                        label = "Pick your measure",
                        choices = c("Counts per Million" = "count_per_million",
                                    "Percentage" = "pct"))