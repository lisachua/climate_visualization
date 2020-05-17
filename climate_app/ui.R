library(shiny)
shinyUI(fluidPage(
  navbarPage("Plots", id="tabs",
  tabPanel(title = "Regression",
           titlePanel("Interactive Regression Plots"),
           sidebarLayout(
             sidebarPanel(
               selectInput('ycol', 'Select a y-axis', 
                           c("Temperature Anomaly (°C)" = "temperature_anomaly", 
                             "CO2 Emissions World Avg (tonnes pp)" = "worldAvg")),
               actionButton("toggle","Toggle Selected Points on Plot"),
               actionButton("reset","Reset All"),
               #actionButton("reset2", "Reset Linear Only"),
               #selectInput("plotType", "Select a type of diagnostic plot", c("Leverage against studentized residuals", "Leverage against DFFITS","Leverage against Residuals")),
               verbatimTextOutput("modelText"),
               verbatimTextOutput("info"),
               uiOutput("plotOptions")
               ),
             mainPanel(
               plotOutput("scatterPlot", click="clickPoint",hover="plot_hover",brush="brushPoints",height="300px"),
               plotOutput("residualPlot",height="300px")
               )
           )
  ),

  #GRAPH2
  tabPanel("Spiralized", 
           titlePanel("Historical World Average Temperature Anomalies by CO2 Emissions"),
           sidebarLayout(
             sidebarPanel(
               #YearSlider
               sliderInput("year",
                           "Current Year",
                           min = 1880,
                           max = 2009,
                           value = 1880,
                           sep="",
                           animate=animationOptions(interval=800)),
               #SelectVariables
               selectInput("colorVar","Select Variable to be Represented by Color",c("CO2 Emissions (tonnes per person)" = "worldAvg","Temperature Anomaly (°C)" = "temperature_anomaly")),
               selectInput("yVar","Select Y Variable",c("Temperature Anomaly (°C)" = "temperature_anomaly","CO2 Emissions (tonnes per person)" = "worldAvg")),
               checkboxInput("legend","Display Legend for Color?")
             ),
             
             # Show a spiralized plot 
             mainPanel(
               plotOutput("spiralizedPlot")
             )
           )
  ),
  
  #GRAPH 3
  tabPanel("World Map",
           titlePanel("CO2 Emissions by Country and Year"),
           sidebarLayout(
            sidebarPanel(
               sliderInput("year2",
                  "Current Year",
                  min = 1880,
                  max = 2009,
                  value = 1880,
                  sep="",
                  animate=animationOptions(interval=3000))
               ),
            mainPanel(
              plotOutput("mapPlot")
              )
            )
          )
  )
))