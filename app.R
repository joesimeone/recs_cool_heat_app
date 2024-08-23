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



## Scripts to help server side calls ---------------------------
source("scripts/source_data.R")
source("scripts/graph_filter.R")
source("scripts/create_st_ac_graph.R")
source("scripts/create_leaflet_maps.R")

## Loading objects for UI ----------------------------------------
state_search <- selectizeInput("state_search",
                               c("Look up your state" = ""),
                               unique(st_tbl$states))

measure <- radioButtons(inputId = "measure_type",
                        label = "Pick your measure",
                        choices = c("Counts per Million" = "Counts Per Million",
                                    "Percentage" = "Percentage"))

cool_type <- selectizeInput("cool_search",
                            c("Pick an Indicator" = ""),
                            unique(st_tbl$cool_type_fac))

cards <- list( 
              ac_plot = card(
                full_screen = TRUE,
                         card_header(tags$div(
                           "Access to Home Cooling, 2020",
                           style = "text-align: center;",
                           class = "align-items-center"
                            )
                        ),
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
               
                  nav_panel(
                    tags$div("RMSE Map", 
                             style = "text-align: center;",
                             class = "align-items-center"),
                            layout_sidebar(
                              sidebar = sidebar(
                                cool_type,
                                measure
                              ),
                            leafletOutput("rse_map"))),
                  nav_panel("Estimates Map",
                            layout_sidebar(
                              sidebar = sidebar(
                                cool_type,
                                measure
                              ),
                            leafletOutput("est_map"))
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

    filter_graph(st_tbl, states, stat,
                 input$state_search,
                 input$measure_type)


    

    })
  
  maps_filter <- reactive({

    req(input$cool_search)
    req(input$measure_type)

    filter_graph(st_tbl_shp, cool_type_fac, stat,
                 input$cool_search,
                 input$measure_type)




  })

  

  
  
  output$st_graph <- renderPlotly({
    
    #validate(need(nrow(state_filter()) > 0, "No data available for the selected inputs."))
                       
                    render_ac_graph(state_filter(), input$measure_type)
                     
                    })
                    


output$rse_map <- renderLeaflet({
  
  #validate(need(nrow(state_filter()) > 0, "No data available for the selected inputs."))
  
  render_rse_map(maps_filter())
  
})



output$est_map <- renderLeaflet({
  
  #validate(need(nrow(state_filter()) > 0, "No data available for the selected inputs."))
  
  render_est_map(maps_filter())
  
})

}

# Run the application 
shinyApp(ui = ui, server = server)
