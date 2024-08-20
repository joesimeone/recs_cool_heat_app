library("tidyverse")



filter_graph <- function(var1, var2, filt1, filt2){
  

  st_tbl %>% 
   filter({{ var1 }} %in% filt1) %>% 
   filter({{ var2 }} %in% filt2)
}
