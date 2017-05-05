library('shiny')
library('shinydashboard')
library('shinythemes')
#source("oil_stock_prediction.R")
#source("sdmprojectNBRcode.R")

header <- dashboardHeader(title = "Delta prediction")

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
                                      verbatimTextOutput("summary")),
                                  box(title = "Table view", div(style = 'overflow-x: scroll', tableOutput("view")))
                                )
                        ),
                        tabItem(tabName = "Models",
                                box(title = "Models", selectInput("Models", "Choose a Model:", choices = c("NaiveBayes", "Linear Regression", "Logistic Regression", "R-Part", "GBM")))
                        )
                      )
)
ui <- dashboardPage(header, sidebar, body)

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
  # Show the first "n" observations
  output$view <- renderTable({head(datasetInput(), n = input$obs)},include.rownames=TRUE, digits=6)
})

header <- dashboardHeader(title = "Delta prediction")

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

