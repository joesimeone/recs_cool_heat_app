library(MetBrewer)
library(plotly)



render_ac_graph <- function(dat, est_lab){

                  

color <- c("black", "#a40000", "#16317d", "#007e2f", "#e78429")

     bones <-   ggplot(dat, aes(x = reorder(cool_type_fac, estimates), 
                       y = estimates,
                       fill = cool_type_fac, color = cool_type_fac,
                       text = paste(
                                    "Cooling:", cool_type_fac, "<br>",
                                     "Upper bound:", round(conf_int_upper, 2), "<br>",
                                    "Estimate:", round(estimates, 2), "<br>",
                                     "Lower bound:", round(conf_int_lower, 2), "<br>",
                                    "RSE:", round(estimates_rse, 2), "<br>")
                            )) +         
              geom_point(color = "white", shape = 21, size = 2.5) +
              geom_errorbar(aes(ymin = conf_int_lower, ymax = conf_int_upper), 
                            linetype = "dashed", width = .1) + 
              theme(legend.position="none",
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank(),
                    panel.grid.major.x = element_line(color = "gray", 
                                                      linetype = "dashed", 
                                                      linewidth = .15),
                    panel.background = element_rect(fill = "ghostwhite")) +
                    xlab("") +
                    ylab(est_lab) +
                  
  
                  coord_flip() + 
                  scale_fill_manual(values = color) +
                  scale_color_manual(values = color) 

bones_plotly <- ggplotly(bones)

return(bones_plotly)}






