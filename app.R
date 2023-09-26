# Load required libraries
library(shiny)
library(ggplot2)
library(DT)
library(scales)
library(dplyr)

# Define UI
ui <- fluidPage(
  tags$style(type='text/css', ".sidebar-panel { width: 15px; !important;}"),
  # App title
  titlePanel("Colonization Time Estimator"),
  
  # Compact sidebar with numericInputs for exact values
  sidebarLayout(
    sidebarPanel(
      id='sidebar',
      width = 2,
      sliderInput("number_of_stars", "Number of Stars",
                  min = 10e9, max = 1000e9, value = 570e9, step = 10e9,ticks=F),
      sliderInput("fraction_habitable", "Fraction of Stars with Habitable Planets",
                  min = 0, max = 1, value = 0.01, step = 0.001, ticks=F),
      sliderInput("ship_speed", "Speed of Ship (% of light speed)",
                  min = 0.0006, max = 0.3, value = 0.0006, step = 0.001),
      sliderInput("years_to_build_ship", "Years to Build Each Ship",
                  min = 1, max = 5000, value = 1000, step = 1, ticks=F),
      sliderInput("startup_lag", "Startup Lag (Years)",
                  min = 1, max = 50000, value = 4000, step = 1, ticks=F),
      sliderInput("avg_distance_ly", "Average Distance Between Stars (light years)",
                  min = 1, max = 50, value = 10, step = 1)
    ),
    
    # Main panel for displaying outputs with tabs
    mainPanel(
      h3(textOutput("summary")),
      tabsetPanel(
        tabPanel("Plot", plotOutput("colonizationPlot")),
        tabPanel("Data", DTOutput("colonizationTable"))
      )
    )
  )
)

# Define server
server <- function(input, output) {
  
  source("functions.R")
  
  colonization_data <- reactive({
    colonization_time(
      number_of_stars = input$number_of_stars,
      fraction_habitable = input$fraction_habitable,
      ship_speed = input$ship_speed,
      years_to_build_ship = input$years_to_build_ship,
      startup_lag = input$startup_lag,
      avg_distance_ly = input$avg_distance_ly
    )
  })
  
  output$summary <- renderText({
    df <- colonization_data()
    total_years <- df$years[length(df$years)]
    habitable <- input$number_of_stars * input$fraction_habitable
    travel_time <- input$avg_distance_ly / input$ship_speed
    
    paste0("Total Years to Colonize: ", comma_format()(total_years),
           " | Average Travel Time: ", comma_format()(travel_time), " years",
           " | Habitable Planets: ", comma_format()(habitable))
  })
  
  output$colonizationPlot <- renderPlot({
    
    df <- colonization_data()
    
    ggplot(df, aes(x = years)) +
      geom_line(aes(y = colonized_planets, color = "Colonized Planets")) +
      geom_line(aes(y = planets_startup_lag, color = "Planets in Startup Lag")) +
      geom_line(aes(y = planets_building_ship, color = "Planets Building Ship")) +
      geom_line(aes(y = ships_in_transit, color = "Ships in Transit")) +
      labs(y = "Counts", color = "Legend") +
      theme_minimal()
    
  })
  
  output$colonizationTable <- renderDT({
    colonization_data()},
    options = list(pageLength = -1) # Non-paged, infinitely scrolling
  )
  
}

# Run the app
shinyApp(ui = ui, server = server)


# # Load required libraries
# library(shiny)
# library(ggplot2)
# library(DT)
# 
# # Define UI
# ui <- fluidPage(
#   
#   # App title
#   titlePanel("Colonization Time Estimator"),
#   
#   # Sidebar with sliders for inputs
#   sidebarLayout(
#     sidebarPanel(
# sliderInput("number_of_stars", "Number of Stars",
#             min = 10e9, max = 1000e9, value = 570e9, step = 10e9),
# sliderInput("fraction_habitable", "Fraction of Stars with Habitable Planets",
#             min = 0, max = 1, value = 0.01, step = 0.001),
# sliderInput("ship_speed", "Speed of Ship (% of light speed)",
#             min = 0.01, max = 1, value = 0.1, step = 0.01),
# sliderInput("years_to_build_ship", "Years to Build Each Ship",
#             min = 1, max = 100, value = 10, step = 1),
# sliderInput("startup_lag", "Startup Lag (Years)",
#             min = 1, max = 2000, value = 20, step = 1),
# sliderInput("avg_distance_ly", "Average Distance Between Stars (light years)",
#             min = 1, max = 50, value = 10, step = 1)
#     ),
#     
#     # Main panel for displaying outputs with tabs
#     mainPanel(
#       tabsetPanel(
#         tabPanel("Plot", plotOutput("colonizationPlot")),
#         tabPanel("Data", DTOutput("colonizationTable"))
#       )
#     )
#   )
# )
# 
# # Define server
# server <- function(input, output) {
#   
#   source("functions.R")
#   
#   colonization_data <- reactive({
#     colonization_time(
#       number_of_stars = input$number_of_stars,
#       fraction_habitable = input$fraction_habitable,
#       ship_speed = input$ship_speed,
#       years_to_build_ship = input$years_to_build_ship,
#       startup_lag = input$startup_lag,
#       avg_distance_ly = input$avg_distance_ly
#     )
#   })
#   
#   output$colonizationPlot <- renderPlot({
#     
#     df <- colonization_data()
#     
#     ggplot(df, aes(x = years)) +
#       geom_line(aes(y = colonized_planets, color = "Colonized Planets")) +
#       geom_line(aes(y = planets_startup_lag, color = "Planets in Startup Lag")) +
#       geom_line(aes(y = planets_building_ship, color = "Planets Building Ship")) +
#       geom_line(aes(y = ships_in_transit, color = "Ships in Transit")) +
#       labs(y = "Counts", color = "Legend") +
#       theme_minimal()
#     
#   })
#   
#   output$colonizationTable <- renderDT({
#     colonization_data()
#   })
#   
# }
# 
# # Run the app
# shinyApp(ui = ui, server = server)
# 
# 
# # # Load required libraries
# # library(shiny)
# # library(ggplot2)
# # library(dplyr)
# # 
# # # Define UI
# # ui <- fluidPage(
# #   
# #   # App title
# #   titlePanel("Colonization Time Estimator"),
# #   
# #   # Sidebar with sliders for inputs
# #   sidebarLayout(
# #     sidebarPanel(
# #       sliderInput("number_of_stars", "Number of Stars", 
# #                   min = 10e9, max = 1000e9, value = 570e9, step = 10e9),
# #       sliderInput("fraction_habitable", "Fraction of Stars with Habitable Planets", 
# #                   min = 0, max = 1, value = 0.01, step = 0.001),
# #       sliderInput("ship_speed", "Speed of Ship (% of light speed)", 
# #                   min = 0.01, max = 1, value = 0.1, step = 0.01),
# #       sliderInput("years_to_build_ship", "Years to Build Each Ship", 
# #                   min = 1, max = 100, value = 10, step = 1),
# #       sliderInput("startup_lag", "Startup Lag (Years)", 
# #                   min = 1, max = 2000, value = 20, step = 1),
# #       sliderInput("avg_distance_ly", "Average Distance Between Stars (light years)", 
# #                   min = 1, max = 50, value = 10, step = 1)
# #     ),
# #     
# #     # Main panel for displaying outputs
# #     mainPanel(
# #       plotOutput("colonizationPlot")
# #     )
# #   )
# # )
# # 
# # # Define server
# # server <- function(input, output) {
# #   
# #   source("functions.R")
# #   
# #   output$colonizationPlot <- renderPlot({
# #     
# #     df <- colonization_time(
# #       number_of_stars = input$number_of_stars,
# #       fraction_habitable = input$fraction_habitable,
# #       ship_speed = input$ship_speed,
# #       years_to_build_ship = input$years_to_build_ship,
# #       startup_lag = input$startup_lag,
# #       avg_distance_ly = input$avg_distance_ly
# #     )
# #     
# #     ggplot(df, aes(x = years)) +
# #       geom_line(aes(y = colonized_planets, color = "Colonized Planets")) +
# #       geom_line(aes(y = planets_startup_lag, color = "Planets in Startup Lag")) +
# #       geom_line(aes(y = planets_building_ship, color = "Planets Building Ship")) +
# #       geom_line(aes(y = ships_in_transit, color = "Ships in Transit")) +
# #       labs(y = "Counts", color = "Legend") +
# #       theme_minimal()
# #     
# #   })
# #   
# # }
# # 
# # # Run the app
# # shinyApp(ui = ui, server = server)
