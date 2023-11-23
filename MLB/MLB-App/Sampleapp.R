# Loads in the necessary libraries
library(shiny)
library(shinydashboard)
library(rsconnect)
library(DT)
library(formattable)
library(tidyverse)
library(hrbrthemes)
library(viridis)


# Loads in the files used
source("appMLBData.R")



# Define UI
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("MLB Reports", tabName = "MLB_tab", icon = icon("baseball-ball"),
               menuSubItem("MLB Pitches Overview", tabName = "MLBAll_tab", icon = icon("baseball-ball")),
               menuSubItem("MLB Pitch Table", tabName = "MLBPitch_tab", icon = icon("baseball-ball")),
               menuSubItem("MLB Pitchers", tabName = "MLBName_tab", icon = icon("baseball-ball")),
               menuSubItem("MLB Starters", tabName = "MLBNameStart_tab", icon = icon("baseball-ball"))
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "MLBAll_tab",
              fluidRow(
                selectInput("name_select1", "Select Name:", choices = unique(MLBleaguePitch$Name)),
                column(width = 5, plotOutput("MLBAll_horz_IVB_plot")),
                column(width = 5, plotOutput("MLBAll_velocity_spin_plot")),
                column(width = 5, plotOutput("MLBAll_release_side_height_plot")),
                column(width = 5, plotOutput("MLBAll_plate_side_height_plot"))
              ),
              DTOutput("data_table_MLBAll"),
              plotOutput("MLBAllLineplot"),
              radioButtons("MLB", "Choose a variable:",
                           choices = c("Velo", "SpinRate", "IVB", "HB", "RelHeight", "Extension", "Stuff+", "AVGExitVelo", "xwOBA", "RunValue", "CS%", "SwStr%", "CSW%", "RV/100"),
                           selected = "Velo")
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  MLBpitch = reactive({
    MLBpitches %>%
      filter(Name == input$name_select1)
  })
  
  output$MLBAll_plate_side_height_plot <- renderPlot({
    ggplot(MLBpitch(), aes(x = plate_x, y = plate_z)) +
      geom_hex(bins = 20, fill = "red", color = "blue") +
      geom_segment(x = -17/24, y = 1.5, xend = 17/24, yend = 1.5, color = "black") +
      geom_segment(x = -17/24, y = 1.5, xend = -17/24, yend = 3.5, color = "black") +
      geom_segment(x = -17/24, y = 3.5, xend = 17/24, yend = 3.5, color = "black") +
      geom_segment(x = 17/24, y = 1.5, xend = 17/24, yend = 3.5, color = "black") +
      xlim(-4, 4) +
      ylim(-1, 7) +
      labs(x = "Plate Side (ft)", y = "Plate Height (ft)", title = "Pitch Location") +
      theme_minimal()
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)