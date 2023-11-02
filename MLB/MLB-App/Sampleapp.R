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
  MLBLineData = reactive({
    MLBGrouped %>%
      filter(Name == input$name_select1)
    })
  
  output$MLBAllLineplot <- renderPlot({
    if (input$MLB == "Velo") {
      ggplot(MLBLineData(), aes(x = game_date, y = Velo, color = PitchType)) +
        geom_line() +
        scale_color_manual(values = rainbow(length(unique(MLBLineData()$`PitchType`)))) +
        theme_minimal()
    } else if (input$MLB == "SpinRate") {
      ggplot(MLBLineData(), aes(x = game_date, y = SpinRate, color = PitchType)) +
        geom_line() +
        scale_color_manual(values = rainbow(length(unique(MLBLineData()$`PitchType`)))) +
        theme_minimal()
    } else if (input$MLB == "IVB") {
      ggplot(MLBLineData(), aes(x = game_date, y = IVB, color = PitchType)) +
        geom_line() +
        scale_color_manual(values = rainbow(length(unique(MLBLineData()$`PitchType`)))) +
        theme_minimal()
    } else if (input$MLB == "HB") {
      ggplot(MLBLineData(), aes(x = game_date, y = HB, color = PitchType)) +
        geom_line() +
        scale_color_manual(values = rainbow(length(unique(MLBLineData()$`PitchType`)))) +
        theme_minimal()
    } else if (input$MLB == "RelHeight") {
      ggplot(MLBLineData(), aes(x = game_date, y = RelHeight, color = PitchType)) +
        geom_line() +
        scale_color_manual(values = rainbow(length(unique(MLBLineData()$`PitchType`)))) +
        theme_minimal()
    } else if (input$MLB == "Extension") {
      ggplot(MLBLineData(), aes(x = game_date, y = Extension, color = PitchType)) +
        geom_line() +
        scale_color_manual(values = rainbow(length(unique(MLBLineData()$`PitchType`)))) +
        theme_minimal()
    } else if (input$MLB == "Stuff+") {
      ggplot(MLBLineData(), aes(x = game_date, y = `Stuff+`, color = PitchType)) +
        geom_line() +
        scale_color_manual(values = rainbow(length(unique(MLBLineData()$`PitchType`)))) +
        theme_minimal()
    } else if (input$MLB == "AVGExitVelo") {
      ggplot(MLBLineData(), aes(x = game_date, y = AVGExitVelo, color = PitchType)) +
        geom_line() +
        scale_color_manual(values = rainbow(length(unique(MLBLineData()$`PitchType`)))) +
        theme_minimal()
    } else if (input$MLB == "xwOBA") {
      ggplot(MLBLineData(), aes(x = game_date, y = xwOBA, color = PitchType)) +
        geom_line() +
        scale_color_manual(values = rainbow(length(unique(MLBLineData()$`PitchType`)))) +
        theme_minimal()
    } else if (input$MLB == "RunValue") {
      ggplot(MLBLineData(), aes(x = game_date, y = RunValue, color = PitchType)) +
        geom_line() +
        scale_color_manual(values = rainbow(length(unique(MLBLineData()$`PitchType`)))) +
        theme_minimal()
    } else if (input$MLB == "CS%") {
      ggplot(MLBLineData(), aes(x = game_date, y = CS, color = PitchType)) +
        geom_line() +
        scale_color_manual(values = rainbow(length(unique(MLBLineData()$`PitchType`)))) +
        theme_minimal()
    } else if (input$MLB == "SwStr%") {
      ggplot(MLBLineData(), aes(x = game_date, y = SwStr, color = PitchType)) +
        geom_line() +
        scale_color_manual(values = rainbow(length(unique(MLBLineData()$`PitchType`)))) +
        theme_minimal()
    } else if (input$MLB == "CSW%") {
      ggplot(MLBLineData(), aes(x = game_date, y = CSW, color = PitchType)) +
        geom_line() +
        scale_color_manual(values = rainbow(length(unique(MLBLineData()$`PitchType`)))) +
        theme_minimal()
    } else {
      ggplot(MLBLineData(), aes(x = game_date, y = RV100, color = PitchType)) +
        geom_line() +
        scale_color_manual(values = rainbow(length(unique(MLBLineData()$`PitchType`)))) +
        theme_minimal()
    } 
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
