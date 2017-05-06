## SHINY R###
#reading file

jetfuel <- read.csv(file = 'jetfuelprices.csv', skip = 12, header = T, as.is = T, na.strings = "#N/A")
delta <- read.csv(file = 'Deltastocks.csv', skip = 0, header = T, as.is = T, na.strings = "NA")
sp500 <- read.csv(file = 'Proj-SP500.csv', skip = 0, header = T, as.is = T, na.strings = "NA")
brentoil <- read.csv(file = 'brentoilprices.csv', skip = 12, header = T, as.is = T, na.strings = c("NA", "#N/A" ))


header <- dashboardHeader(title = "Delta prediction", titleWidth = 100)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Models", tabName = "Models", icon = icon("th"), badgeLabel = "new", badgeColor = "green")
  )
)

body <- dashboardBody(theme = "bootstrap.css",
                      tabItems(
                        tabItem(tabName = "dashboard",
                                fluidRow(
                                  box(solidHeader = TRUE, selectInput("dataset", "Choose a dataset:", choices = c("Brent Oil", "Jet Fuel", "delta", "S$P500 Index", "Workdataset")),
                                      numericInput("obs", "Number of observations to view:", 20),
                                      verbatimTextOutput(outputId = "summary")),
                                  box(title = "Table view", div(style = 'overflow-x: scroll', tableOutput("view")))
                                )
                        ),
                        tabItem(tabName = "Models",
                                fluidRow(
                                  box(solidHeader = TRUE, selectInput("selmodel", "Choose a Model:", choices = c("NaiveBayes", "Linear Regression", "Logistic Regression", "R-Part", "GBM")),
                                      plotOutput(outputId = "summaryModels")),
                                  box(title = "Plot View", solidHeader = TRUE,  verbatimTextOutput(outputId = "predY"))
                                )   
                        )
                      )
)
ui <- dashboardPage(header, sidebar, body)



### LOAD LIBRARIES AND SOURCE FILES ###

library('shiny')
library('shinydashboard')
library('shinythemes')
library('sqldf')
library("e1071")
library("caret")
library('rpart')
library('gbm')
library('klaR')
library('MASS')

#source("dfcreation.R")
#source("workdscreation.R")
#source('linearReg.R')
#source("logitReg.R") 
#source('rpartReg.R') 
#source('gbmReg.R') 
#source('NBRcode.R')
### LOAD LIBRARIES AND SOURCE FILES ENDS ###

server <- shinyServer(function(input, output){  
  # Return the requested dataset
  datasetInput <- reactive({    
    switch(input$dataset, "Brent Oil" = brentoil, "Jet Fuel" = jetfuel, "delta" = delta, "S$P500 Index" = sp500, "Workdataset" = workdata)
  })
  # Generate a summary of the dataset
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })  
  
  modelsInput <- reactive({    
    switch(input$selmodel, "NaiveBayes" = nb , "Linear Regression"= linear.reg, "Logistic Regression" = lr.delta.01, "R-Part" = model.rpt, "GBM" = model.gbm)
  })
  
  output$predY <- renderPrint({
    y <- modelsInput()
    print(y)
  }) 
  
  output$summaryModels <- renderPlot({
    plot(linear.predict, test$delta_pc1) 
  })
  # Show the first "n" observations
  output$view <- renderTable({head(datasetInput(), n = input$obs)},include.rownames=TRUE, digits=6)
})

shinyApp(ui = ui, server = server)