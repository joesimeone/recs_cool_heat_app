#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(plotly)
library(reactable)
library(leaflet)

st_tbl <- read_csv(here("data", "st_tbl_fin.csv"))


st_tbl <- st_tbl %>% 
  filter(cool_type != "all_cool_tot_per_mil") %>% 
  mutate(cool_type_fac = 
           case_when(
             str_detect(cool_type, "wall_window") ~ "Wall or Window Unit",
             str_detect(cool_type, "dehumidifier") ~ "Dehumidifer",
             str_detect(cool_type, "central_ac") ~ "Central Air",
             str_detect(cool_type, "ac_tot") | 
               str_detect(cool_type, "any_ac") ~ "Any AC",
             str_detect(cool_type, "ceiling_fan") ~ "Uses Ceiling Fan")) 




## Scripts to help server side calls ---------------------------
source("scripts/graph_filter.R")
source("scripts/create_st_ac_graph.R")

## Loading objects for UI ----------------------------------------
state_search <- selectizeInput("state_search",
                               c("Look up your state" = ""),
                               unique(st_tbl$states))

measure <- radioButtons(inputId = "measure_type",
                        label = "Pick your measure",
                        choices = c("Counts per Million" = "count_per_million",
                                    "Percentage" = "pct"))

cool_type <- selectizeInput("cool_search",
                            c("Pick an Indicator" = ""),
                            unique(st_tbl$cool_type_fac))

cards <- list(
              ac_plot = card(
                full_screen = TRUE,
                         card_header("Access to Home Cooling, 2020"),
                            layout_sidebar(
                              sidebar = sidebar(
                                state_search,
                                measure
                              ),
                                card_body(
                                  plotlyOutput("st_graph")
                                 
                              )
                              )
                ),
              ac_map = navset_card_underline(
                # layout_sidebar(
                #   sidebar = sidebar(
                #               cool_type,
                #               measure
                #   ),
                  nav_panel("RMSE Map", 
                            layout_sidebar(
                              sidebar = sidebar(
                                cool_type,
                                measure
                              ),
                            leafletOutput("rmse_map")),
                  nav_panel("Estimates Map", leafletOutput("est_map"))
                )
              )
)
                                




# Define UI for application that draws a histogram
ui <- page_fluid(
                
  cards[[1]],
  cards[[2]])

        # card(
        #       height = 250,
        #       card_header("Test"),
        #         layout_sidebar(
        #             sidebar = sidebar(
        #                         state_search,
        #                         measure
        #                        ),
        #    card_body(
        #             plotlyOutput("st_graph"))
        #    ),
        #    
        # )
       


            
 



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # state_filter <- reactive({
  #   
  #   req(input$state_search)
  #   req(input$measure_type)
  #   
  #   st_tbl %>% filter(states %in% input$state_search) %>% 
  #              filter(stat %in% input$measure_type)})
  

  state_filter <- reactive({
    
    req(input$state_search)
    req(input$measure_type)

    filter_graph(states, stat,
                 input$state_search,
                 input$measure_type)


    

    })
  

  
  
  output$st_graph <- renderPlotly({
    
    #validate(need(nrow(state_filter()) > 0, "No data available for the selected inputs."))
                       
                    render_ac_graph(state_filter(), input$measure_type)
                     
                    })
                    
}

# Run the application 
shinyApp(ui = ui, server = server)
