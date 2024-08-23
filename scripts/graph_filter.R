library("tidyverse")



filter_graph <- function(dat, var1, var2,
                         filt1, filt2){
  

  dat %>% 
   filter({{ var1 }} %in% filt1) %>% 
   filter({{ var2 }} %in% filt2)
}
