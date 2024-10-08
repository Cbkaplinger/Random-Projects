#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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


# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("MLB Reports", tabName = "MLB_tab", icon = icon("baseball-ball"),
               menuSubItem("MLB Pitches Overview", tabName = "MLBAll_tab", icon = icon("baseball-ball")),
               menuSubItem("MLB Pitch Table", tabName = "MLBPitch_tab", icon = icon("baseball-ball")),
               menuSubItem("MLB Pitchers", tabName = "MLBName_tab", icon = icon("baseball-ball")),
               menuSubItem("MLB Starters", tabName = "MLBNameStart_tab", icon = icon("baseball-ball"))
               ),
      menuItem("AL Reports", tabName = "AL_tab", icon = icon("baseball-ball"),
               menuSubItem("AL Pitches Overview", tabName = "ALAll_tab", icon = icon("baseball-ball")),
               menuSubItem("AL Pitch Table", tabName = "ALPitch_tab", icon = icon("baseball-ball")),
               menuSubItem("AL Pitchers", tabName = "ALName_tab", icon = icon("baseball-ball")),
               menuSubItem("AL Starters", tabName = "ALNameStart_tab", icon = icon("baseball-ball"))
               ),
      menuItem("NL Reports", tabName = "NL_tab", icon = icon("baseball-ball"),
               menuSubItem("NL Pitches Overview", tabName = "NLAll_tab", icon = icon("baseball-ball")),
               menuSubItem("NL Pitch Table", tabName = "NLPitch_tab", icon = icon("baseball-ball")),
               menuSubItem("NL Pitchers", tabName = "NLName_tab", icon = icon("baseball-ball")),
               menuSubItem("NL Starters", tabName = "NLNameStart_tab", icon = icon("baseball-ball"))
               ),
      menuItem("Glossary", tabName = "Glossary_tab", icon = icon("baseball-ball"))
      )
    ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "MLBAll_tab",
              fluidRow(
                selectInput("name_select1", "Select Name:", choices = unique(MLBleaguePitch$Name)),
                column(width = 6, plotOutput("MLBAll_horz_IVB_plot")),
                column(width = 6, plotOutput("MLBAll_velocity_spin_plot")),
                column(width = 6, plotOutput("MLBAll_release_side_height_plot")),
                column(width = 6, plotOutput("MLBAll_plate_side_height_plot"))
                ),
              DTOutput("data_table_MLBAll"),
              column(width = 12, plotOutput("MLBAllLineplot")),
              radioButtons("MLB", "Choose a variable:",
                           choices = c("Velo", "SpinRate", "IVB", "HB", "RelHeight", "Extension", "Stuff+", "xwOBA", "RunValue", "RV/100"),
                           selected = "Velo")
              ),
      tabItem(tabName = "MLBPitch_tab",
              DTOutput("data_table_MLBAll2")
              ),
      tabItem(tabName = "MLBName_tab",
              DTOutput("data_table_MLBName")
              ),
      tabItem(tabName = "MLBNameStart_tab", 
              DTOutput("data_table_MLBNameStart")
              ),
      tabItem(tabName = "ALAll_tab",
              fluidRow(
                selectInput("name_select2", "Select Name:", choices = unique(ALleaguePitch$Name)),
                column(width = 6, plotOutput("ALAll_horz_IVB_plot")),
                column(width = 6, plotOutput("ALAll_velocity_spin_plot")),
                column(width = 6, plotOutput("ALAll_release_side_height_plot")),
                column(width = 6, plotOutput("ALAll_plate_side_height_plot"))
              ),
              DTOutput("data_table_ALAll"),
              column(width = 12, plotOutput("ALAllLineplot")),
              radioButtons("AL", "Choose a variable:",
                           choices = c("Velo", "SpinRate", "IVB", "HB", "RelHeight", "Extension", "Stuff+", "xwOBA", "RunValue", "RV/100"),
                           selected = "Velo")
              ),
      tabItem(tabName = "ALPitch_tab",
              DTOutput("data_table_ALAll2")
              ),
      tabItem(tabName = "ALName_tab",
              DTOutput("data_table_ALName")
              ),
      tabItem(tabName = "ALNameStart_tab",
              DTOutput("data_table_ALNameStart")
              ),
      tabItem(tabName = "NLAll_tab",
              fluidRow(
                selectInput("name_select3", "Select Name:", choices = unique(NLleaguePitch$Name)),
                column(width = 6, plotOutput("NLAll_horz_IVB_plot")),
                column(width = 6, plotOutput("NLAll_velocity_spin_plot")),
                column(width = 6, plotOutput("NLAll_release_side_height_plot")),
                column(width = 6, plotOutput("NLAll_plate_side_height_plot"))
              ),
              DTOutput("data_table_NLAll"),
              column(width = 12, plotOutput("NLAllLineplot")),
              radioButtons("NL", "Choose a variable:",
                           choices = c("Velo", "SpinRate", "IVB", "HB", "RelHeight", "Extension", "Stuff+", "xwOBA", "RunValue", "RV/100"),
                           selected = "Velo")
              ),
      tabItem(tabName = "NLPitch_tab",
              DTOutput("data_table_NLAll2")
              ),
      tabItem(tabName = "NLName_tab",
              DTOutput("data_table_NLName")
      ),
      tabItem(tabName = "NLNameStart_tab",
              DTOutput("data_table_NLNameStart")
              )
      )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  MLBpitch = reactive({
    MLBpitches %>%
      filter(Name == input$name_select1)
    })
  
  MLBAll = reactive({
    MLBleaguePitch %>%
      filter(Name == input$name_select1)
    })
  
  ALpitch = reactive({
    ALpitches %>%
      filter(Name == input$name_select2)
    })
  
  ALAll = reactive({
    ALleaguePitch %>%
      filter(Name == input$name_select2)
    })
  
  NLpitch = reactive({
    NLpitches %>%
      filter(Name == input$name_select3)
  })
  
  NLAll = reactive({
    NLleaguePitch %>%
      filter(Name == input$name_select3)
  })
  
  MLBAll2 = reactive({
    MLBleaguePitch
  })
  
  MLBName = reactive({
    MLBleagueName
    })
  
  MLBNameStart = reactive({
    MLBleagueNameStart
    })
  
  ALAll2 = reactive({
    ALleaguePitch
  })
  
  ALName = reactive({
    ALleagueName
    })
  
  ALNameStart = reactive({
    ALleagueNameStart
    })
  
  NLAll2 = reactive({
    NLleaguePitch
  })
  
  NLName = reactive({
    ALleagueName
  })
  
  NLNameStart = reactive({
    NLleagueNameStart
  })
  
  MLBLineData = reactive({
    MLBGrouped %>%
      filter(Name == input$name_select1)
  })
  
  ALLineData = reactive({
    ALGrouped %>%
      filter(Name == input$name_select2)
  })
  
  NLLineData = reactive({
    NLGrouped %>%
      filter(Name == input$name_select3)
  })
  
  # Render the tables
  output$data_table_MLBAll <- DT::renderDataTable({
    datatable(MLBAll(), options = list(scrollX = TRUE, pageLength = 10))
    })
  
  output$data_table_MLBAll2 <- DT::renderDataTable({
    datatable(MLBAll2(), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  output$data_table_MLBName <- DT::renderDataTable({
    datatable(MLBName(), options = list(scrollX = TRUE, pageLength = 10))
    })
  
  output$data_table_MLBNameStart <- DT::renderDataTable({
    datatable(MLBNameStart(), options = list(scrollX = TRUE, pageLength = 10))
    })
  
  output$data_table_ALAll <- DT::renderDataTable({
    datatable(ALAll(), options = list(scrollX = TRUE, pageLength = 10))
    })
  
  output$data_table_ALAll2 <- DT::renderDataTable({
    datatable(ALAll2(), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  output$data_table_ALName <- DT::renderDataTable({
    datatable(ALName(), options = list(scrollX = TRUE, pageLength = 10))
    })
  
  output$data_table_ALNameStart <- DT::renderDataTable({
    datatable(ALNameStart(), options = list(scrollX = TRUE, pageLength = 10))
    })
  
  output$data_table_NLAll <- DT::renderDataTable({
    datatable(NLAll(), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  output$data_table_NLAll2 <- DT::renderDataTable({
    datatable(NLAll2(), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  output$data_table_NLName <- DT::renderDataTable({
    datatable(NLName(), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  output$data_table_NLNameStart <- DT::renderDataTable({
    datatable(NLNameStart(), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  output$MLBAll_horz_IVB_plot <- renderPlot({
    ggplot(MLBpitch(), aes(x = `HB`, y = `IVB`, color = `PitchType`)) +
      geom_point(size = 3) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      xlim(-25, 25) +
      ylim(-25, 25) +
      labs(x = "Horizontal Break (in)", y = "Induced Vertical Break (in)", title = "Movement") +
      scale_color_manual(values = rainbow(length(unique(MLBpitch()$`PitchType`)))) +
      theme_minimal()
  })
  
  output$MLBAll_velocity_spin_plot <- renderPlot({
    ggplot(MLBpitch(), aes(x = release_speed, y = release_spin_rate, color = `PitchType`)) +
      geom_point(size = 3) +
      geom_vline(xintercept = 80, linetype = "dashed") +
      geom_hline(yintercept = 2000, linetype = "dashed") +
      xlim(60, 100) +
      ylim(1000, 3000) +
      labs(x = "Velocity (mph)", y = "Spin Rate (rpm)", title = "Velocity vs Spin Rate") +
      scale_color_manual(values = rainbow(length(unique(MLBpitch()$`PitchType`)))) +
      theme_minimal()
  })
  
  output$MLBAll_release_side_height_plot <- renderPlot({
    ggplot(MLBpitch(), aes(x = RelSide, y = RelHeight, color = `PitchType`)) +
      geom_point(size = 3) +
      geom_vline(xintercept = 0, linetype = "dashed") + 
      geom_hline(yintercept = 4, linetype = "dashed") +
      xlim(-4, 4) +
      ylim(0, 8) +
      labs(x = "Release Side (ft)", y = "Release Height (ft)", title = "Release Point") +
      scale_color_manual(values = rainbow(length(unique(MLBpitch()$`PitchType`)))) +
      theme_minimal()
  })
  
  output$MLBAll_plate_side_height_plot <- renderPlot({
    ggplot(MLBpitch(), aes(x = plate_x, y = plate_z)) +
      geom_hex() +
      scale_fill_gradient(low = "white", high = "red") +
      geom_segment(x = -17/24, y = 1.5, xend = 17/24, yend = 1.5, color = "black") + 
      geom_segment(x = -17/24, y = 1.5, xend = -17/24, yend = 3.5, color = "black") +
      geom_segment(x = -17/24, y = 3.5, xend = 17/24, yend = 3.5, color = "black") +
      geom_segment(x = 17/24, y = 1.5, xend = 17/24, yend = 3.5, color = "black") +
      xlim(-3, 3) +
      ylim(-1, 5) +
      labs(x = "Plate Side (ft)", y = "Plate Height (ft)", title = "Pitch Location", fill = "Count") +
      theme_minimal()
  })
  
  output$MLBAllLineplot <- renderPlot({
    if (input$MLB == "Velo") {
      ggplot(MLBLineData(), aes(x = game_date, y = Velo, color = PitchType)) +
        geom_line() +
        xlab("Date") +
        scale_color_manual(values = rainbow(length(unique(MLBLineData()$`PitchType`)))) +
        theme_minimal()
    } else if (input$MLB == "SpinRate") {
      ggplot(MLBLineData(), aes(x = game_date, y = SpinRate, color = PitchType)) +
        geom_line() +
        xlab("Date") +
        scale_color_manual(values = rainbow(length(unique(MLBLineData()$`PitchType`)))) +
        theme_minimal()
    } else if (input$MLB == "IVB") {
      ggplot(MLBLineData(), aes(x = game_date, y = IVB, color = PitchType)) +
        geom_line() +
        xlab("Date") +
        scale_color_manual(values = rainbow(length(unique(MLBLineData()$`PitchType`)))) +
        theme_minimal()
    } else if (input$MLB == "HB") {
      ggplot(MLBLineData(), aes(x = game_date, y = HB, color = PitchType)) +
        geom_line() +
        xlab("Date") +
        scale_color_manual(values = rainbow(length(unique(MLBLineData()$`PitchType`)))) +
        theme_minimal()
    } else if (input$MLB == "RelHeight") {
      ggplot(MLBLineData(), aes(x = game_date, y = RelHeight, color = PitchType)) +
        geom_line() +
        xlab("Date") +
        scale_color_manual(values = rainbow(length(unique(MLBLineData()$`PitchType`)))) +
        theme_minimal()
    } else if (input$MLB == "Extension") {
      ggplot(MLBLineData(), aes(x = game_date, y = Extension, color = PitchType)) +
        geom_line() +
        xlab("Date") +
        scale_color_manual(values = rainbow(length(unique(MLBLineData()$`PitchType`)))) +
        theme_minimal()
    } else if (input$MLB == "Stuff+") {
      ggplot(MLBLineData(), aes(x = game_date, y = `Stuff+`, color = PitchType)) +
        geom_line() +
        xlab("Date") +
        scale_color_manual(values = rainbow(length(unique(MLBLineData()$`PitchType`)))) +
        theme_minimal()
    } else if (input$MLB == "xwOBA") {
      ggplot(MLBLineData(), aes(x = game_date, y = xwOBA, color = PitchType)) +
        geom_line() +
        xlab("Date") +
        scale_color_manual(values = rainbow(length(unique(MLBLineData()$`PitchType`)))) +
        theme_minimal()
    } else if (input$MLB == "RunValue") {
      ggplot(MLBLineData(), aes(x = game_date, y = RunValue, color = PitchType)) +
        geom_line() +
        xlab("Date") +
        scale_color_manual(values = rainbow(length(unique(MLBLineData()$`PitchType`)))) +
        theme_minimal()
    } else {
      ggplot(MLBLineData(), aes(x = game_date, y = RV100, color = PitchType)) +
        geom_line() +
        xlab("Date") +
        scale_color_manual(values = rainbow(length(unique(MLBLineData()$`PitchType`)))) +
        theme_minimal()
    } 
  })
  
  output$ALAll_horz_IVB_plot <- renderPlot({
    ggplot(ALpitch(), aes(x = `HB`, y = `IVB`, color = `PitchType`)) +
      geom_point(size = 3) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      xlim(-25, 25) +
      ylim(-25, 25) +
      labs(x = "Horizontal Break (in)", y = "Induced Vertical Break (in)", title = "Movement") +
      scale_color_manual(values = rainbow(length(unique(ALpitch()$`PitchType`)))) +
      theme_minimal()
  })
  output$ALAll_velocity_spin_plot <- renderPlot({
    ggplot(ALpitch(), aes(x = release_speed, y = release_spin_rate, color = `PitchType`)) +
      geom_point(size = 3) +
      geom_vline(xintercept = 80, linetype = "dashed") +
      geom_hline(yintercept = 2000, linetype = "dashed") +
      xlim(60, 100) +
      ylim(1000, 3000) +
      labs(x = "Velocity (mph)", y = "Spin Rate (rpm)", title = "Velocity vs Spin Rate") +
      scale_color_manual(values = rainbow(length(unique(ALpitch()$`PitchType`)))) +
      theme_minimal()
  })
  output$ALAll_release_side_height_plot <- renderPlot({
    ggplot(ALpitch(), aes(x = RelSide, y = RelHeight, color = `PitchType`)) +
      geom_point(size = 3) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      geom_hline(yintercept = 4, linetype = "dashed") +
      xlim(-4, 4) +
      ylim(0, 8) +
      labs(x = "Release Side (ft)", y = "Release Height (ft)", title = "Release Point") +
      scale_color_manual(values = rainbow(length(unique(ALpitch()$`PitchType`)))) +
      theme_minimal()
  })
  
  output$ALAll_plate_side_height_plot <- renderPlot({
    ggplot(ALpitch(), aes(x = plate_x, y = plate_z)) +
      geom_hex() +
      scale_fill_gradient(low = "white", high = "red") +
      geom_segment(x = -17/24, y = 1.5, xend = 17/24, yend = 1.5, color = "black") + 
      geom_segment(x = -17/24, y = 1.5, xend = -17/24, yend = 3.5, color = "black") +
      geom_segment(x = -17/24, y = 3.5, xend = 17/24, yend = 3.5, color = "black") +
      geom_segment(x = 17/24, y = 1.5, xend = 17/24, yend = 3.5, color = "black") +
      xlim(-3, 3) +
      ylim(-1, 5) +
      labs(x = "Plate Side (ft)", y = "Plate Height (ft)", title = "Pitch Location", fill = "Count") +
      theme_minimal()
  })
  
  output$ALAllLineplot <- renderPlot({
    if (input$AL == "Velo") {
      ggplot(ALLineData(), aes(x = game_date, y = Velo, color = PitchType)) +
        geom_line() +
        xlab("Date") +
        scale_color_manual(values = rainbow(length(unique(ALLineData()$`PitchType`)))) +
        theme_minimal()
    } else if (input$AL == "SpinRate") {
      ggplot(ALLineData(), aes(x = game_date, y = SpinRate, color = PitchType)) +
        geom_line() +
        xlab("Date") +
        scale_color_manual(values = rainbow(length(unique(ALLineData()$`PitchType`)))) +
        theme_minimal()
    } else if (input$AL == "IVB") {
      ggplot(ALLineData(), aes(x = game_date, y = IVB, color = PitchType)) +
        geom_line() +
        xlab("Date") +
        scale_color_manual(values = rainbow(length(unique(ALLineData()$`PitchType`)))) +
        theme_minimal()
    } else if (input$AL == "HB") {
      ggplot(ALLineData(), aes(x = game_date, y = HB, color = PitchType)) +
        geom_line() +
        xlab("Date") +
        scale_color_manual(values = rainbow(length(unique(ALLineData()$`PitchType`)))) +
        theme_minimal()
    } else if (input$AL == "RelHeight") {
      ggplot(ALLineData(), aes(x = game_date, y = RelHeight, color = PitchType)) +
        geom_line() +
        xlab("Date") +
        scale_color_manual(values = rainbow(length(unique(ALLineData()$`PitchType`)))) +
        theme_minimal()
    } else if (input$AL == "Extension") {
      ggplot(ALLineData(), aes(x = game_date, y = Extension, color = PitchType)) +
        geom_line() +
        xlab("Date") +
        scale_color_manual(values = rainbow(length(unique(ALLineData()$`PitchType`)))) +
        theme_minimal()
    } else if (input$AL == "Stuff+") {
      ggplot(ALLineData(), aes(x = game_date, y = `Stuff+`, color = PitchType)) +
        geom_line() +
        xlab("Date") +
        scale_color_manual(values = rainbow(length(unique(ALLineData()$`PitchType`)))) +
        theme_minimal()
    } else if (input$AL == "xwOBA") {
      ggplot(ALLineData(), aes(x = game_date, y = xwOBA, color = PitchType)) +
        geom_line() +
        xlab("Date") +
        scale_color_manual(values = rainbow(length(unique(ALLineData()$`PitchType`)))) +
        theme_minimal()
    } else if (input$AL == "RunValue") {
      ggplot(ALLineData(), aes(x = game_date, y = RunValue, color = PitchType)) +
        geom_line() +
        xlab("Date") +
        scale_color_manual(values = rainbow(length(unique(ALLineData()$`PitchType`)))) +
        theme_minimal()
    } else {
      ggplot(ALLineData(), aes(x = game_date, y = RV100, color = PitchType)) +
        geom_line() +
        xlab("Date") +
        scale_color_manual(values = rainbow(length(unique(ALLineData()$`PitchType`)))) +
        theme_minimal()
    } 
  })
  
  output$NLAll_horz_IVB_plot <- renderPlot({
    ggplot(NLpitch(), aes(x = `HB`, y = `IVB`, color = `PitchType`)) +
      geom_point(size = 3) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      xlim(-25, 25) +
      ylim(-25, 25) +
      labs(x = "Horizontal Break (in)", y = "Induced Vertical Break (in)", title = "Movement") +
      scale_color_manual(values = rainbow(length(unique(NLpitch()$`PitchType`)))) +
      theme_minimal()
  })
  output$NLAll_velocity_spin_plot <- renderPlot({
    ggplot(NLpitch(), aes(x = release_speed, y = release_spin_rate, color = `PitchType`)) +
      geom_point(size = 3) +
      geom_vline(xintercept = 80, linetype = "dashed") +
      geom_hline(yintercept = 2000, linetype = "dashed") +
      xlim(60, 100) +
      ylim(1000, 3000) +
      labs(x = "Velocity (mph)", y = "Spin Rate (rpm)", title = "Velocity vs Spin Rate") +
      scale_color_manual(values = rainbow(length(unique(NLpitch()$`PitchType`)))) +
      theme_minimal()
  })
  output$NLAll_release_side_height_plot <- renderPlot({
    ggplot(NLpitch(), aes(x = RelSide, y = RelHeight, color = `PitchType`)) +
      geom_point(size = 3) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      geom_hline(yintercept = 4, linetype = "dashed") +
      xlim(-4, 4) +
      ylim(0, 8) +
      labs(x = "Release Side (ft)", y = "Release Height (ft)", title = "Release Point") +
      scale_color_manual(values = rainbow(length(unique(NLpitch()$`PitchType`)))) +
      theme_minimal()
  })
  output$NLAll_plate_side_height_plot <- renderPlot({
    ggplot(NLpitch(), aes(x = plate_x, y = plate_z)) +
      geom_hex() +
      scale_fill_gradient(low = "white", high = "red") +
      geom_segment(x = -17/24, y = 1.5, xend = 17/24, yend = 1.5, color = "black") + 
      geom_segment(x = -17/24, y = 1.5, xend = -17/24, yend = 3.5, color = "black") +
      geom_segment(x = -17/24, y = 3.5, xend = 17/24, yend = 3.5, color = "black") +
      geom_segment(x = 17/24, y = 1.5, xend = 17/24, yend = 3.5, color = "black") +
      xlim(-3, 3) +
      ylim(-1, 5) +
      labs(x = "Plate Side (ft)", y = "Plate Height (ft)", title = "Pitch Location", fill = "Count") +
      theme_minimal()
  })
  
  output$NLAllLineplot <- renderPlot({
    if (input$NL == "Velo") {
      ggplot(NLLineData(), aes(x = game_date, y = Velo, color = PitchType)) +
        geom_line() +
        xlab("Date") +
        scale_color_manual(values = rainbow(length(unique(NLLineData()$`PitchType`)))) +
        theme_minimal()
    } else if (input$NL == "SpinRate") {
      ggplot(NLLineData(), aes(x = game_date, y = SpinRate, color = PitchType)) +
        geom_line() +
        xlab("Date") +
        scale_color_manual(values = rainbow(length(unique(NLLineData()$`PitchType`)))) +
        theme_minimal()
    } else if (input$NL == "IVB") {
      ggplot(NLLineData(), aes(x = game_date, y = IVB, color = PitchType)) +
        geom_line() +
        xlab("Date") +
        scale_color_manual(values = rainbow(length(unique(NLLineData()$`PitchType`)))) +
        theme_minimal()
    } else if (input$NL == "HB") {
      ggplot(NLLineData(), aes(x = game_date, y = HB, color = PitchType)) +
        geom_line() +
        xlab("Date") +
        scale_color_manual(values = rainbow(length(unique(NLLineData()$`PitchType`)))) +
        theme_minimal()
    } else if (input$NL == "RelHeight") {
      ggplot(NLLineData(), aes(x = game_date, y = RelHeight, color = PitchType)) +
        geom_line() +
        xlab("Date") +
        scale_color_manual(values = rainbow(length(unique(NLLineData()$`PitchType`)))) +
        theme_minimal()
    } else if (input$NL == "Extension") {
      ggplot(NLLineData(), aes(x = game_date, y = Extension, color = PitchType)) +
        geom_line() +
        xlab("Date") +
        scale_color_manual(values = rainbow(length(unique(NLLineData()$`PitchType`)))) +
        theme_minimal()
    } else if (input$NL == "Stuff+") {
      ggplot(NLLineData(), aes(x = game_date, y = `Stuff+`, color = PitchType)) +
        geom_line() +
        xlab("Date") +
        scale_color_manual(values = rainbow(length(unique(NLLineData()$`PitchType`)))) +
        theme_minimal()
    } else if (input$NL == "xwOBA") {
      ggplot(NLLineData(), aes(x = game_date, y = xwOBA, color = PitchType)) +
        geom_line() +
        xlab("Date") +
        scale_color_manual(values = rainbow(length(unique(NLLineData()$`PitchType`)))) +
        theme_minimal()
    } else if (input$NL == "RunValue") {
      ggplot(NLLineData(), aes(x = game_date, y = RunValue, color = PitchType)) +
        geom_line() +
        xlab("Date") +
        scale_color_manual(values = rainbow(length(unique(NLLineData()$`PitchType`)))) +
        theme_minimal()
    } else {
      ggplot(NLLineData(), aes(x = game_date, y = RV100, color = PitchType)) +
        geom_line() +
        xlab("Date") +
        scale_color_manual(values = rainbow(length(unique(NLLineData()$`PitchType`)))) +
        theme_minimal()
    } 
  })
  
}

# Run the application 
shinyApp(ui, server)

