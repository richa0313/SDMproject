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


### R CODE

jetfuel <- read.csv(file = 'jetfuelprices.csv', skip = 12, header = T, as.is = T, na.strings = "#N/A")
delta <- read.csv(file = 'Deltastocks.csv', skip = 0, header = T, as.is = T, na.strings = "NA")
sp500 <- read.csv(file = 'Proj-SP500.csv', skip = 0, header = T, as.is = T, na.strings = "NA")
brentoil <- read.csv(file = 'brentoilprices.csv', skip = 12, header = T, as.is = T, na.strings = c("NA", "#N/A" ))

#checking the NA values
colSums(is.na(jetfuel))
colSums(is.na(delta))
colSums(is.na(sp500))
colSums(is.na(brentoil))

#setting up the dates format and extracting required columns
sp500$Date <- as.Date(sp500$Date, '%Y-%m-%d')
sp500Adj <- sp500[,c('Date','Adj.Close')]
colnames(sp500Adj)[2] <- 'sp500Adj.Close'

delta$Date <- as.Date(delta$Date, '%Y-%m-%d')
deltaAdj <- delta[,c('Date','Adj.Close')]
colnames(deltaAdj)[2] <- 'DeltaAdj.Close'

jetfuel$DATE <- as.Date(jetfuel$DATE, '%Y-%m-%d')
colnames(jetfuel)[2] <- 'JetFuelValue'
brentoil$DATE <- as.Date(brentoil$DATE, '%Y-%m-%d')
colnames(brentoil)[2] <- 'BrentOilValue'

#converting the NAs to mean values of the column - Imputation
jetfuel[is.na(jetfuel[,2]), 2] <- mean(jetfuel[,2], na.rm = TRUE)
brentoil[is.na(jetfuel[,2]), 2] <- mean(brentoil[,2], na.rm = TRUE)

#install.packages('sqldf')
library('sqldf')

finaldata <- sqldf('SELECT *
                   FROM sp500Adj 
                   INNER JOIN deltaAdj ON (sp500Adj.Date = deltaAdj.Date)
                   INNER JOIN brentoil ON (sp500Adj.Date = brentoil.DATE)
                   INNER JOIN jetfuel ON (sp500Adj.Date = jetfuel.DATE)')

#removing redundant date columns 3, 5 and 7
finaldata[3] <- NULL
finaldata[4] <- NULL
finaldata[5] <- NULL
#write.csv(finaldata, "FinalData.csv")
#plot(finaldata$sp500Adj.Close, finaldata$DeltaAdj.Close)

# compute price change for snp500 data
snp_pc1 = c(); snp_pc2= c(); snp_pc3 = c(); snp_pc4 = c(); snp_pc5 = c();
for (i in 7:nrow(finaldata))
{
  snp_pc1[i] <- (finaldata$sp500Adj.Close[i - 1] - finaldata$sp500Adj.Close[i - 2])/finaldata$sp500Adj.Close[i - 2]
  snp_pc2[i] <- (finaldata$sp500Adj.Close[i - 2] - finaldata$sp500Adj.Close[i - 3])/finaldata$sp500Adj.Close[i - 3]
  snp_pc3[i] <- (finaldata$sp500Adj.Close[i - 3] - finaldata$sp500Adj.Close[i - 4])/(finaldata$sp500Adj.Close[i - 4])
  snp_pc4[i] <- (finaldata$sp500Adj.Close[i - 4] - finaldata$sp500Adj.Close[i - 5])/(finaldata$sp500Adj.Close[i - 5])
  snp_pc5[i] <- (finaldata$sp500Adj.Close[i - 5] - finaldata$sp500Adj.Close[i - 6])/(finaldata$sp500Adj.Close[i - 6])
}

# compute price change for brent oil
brent_pc1 = c(); brent_pc2= c(); brent_pc3 = c(); brent_pc4 = c(); brent_pc5 = c();
for (i in 7:nrow(finaldata))
{
  brent_pc1[i] <- (finaldata$BrentOilValue[i - 1] - finaldata$BrentOilValue[i - 2])/(finaldata$BrentOilValue[i - 2])
  brent_pc2[i] <- (finaldata$BrentOilValue[i - 2] - finaldata$BrentOilValue[i - 3])/(finaldata$BrentOilValue[i - 3])
  brent_pc3[i] <- (finaldata$BrentOilValue[i - 3] - finaldata$BrentOilValue[i - 4])/(finaldata$BrentOilValue[i - 4])
  brent_pc4[i] <- (finaldata$BrentOilValue[i - 4] - finaldata$BrentOilValue[i - 5])/(finaldata$BrentOilValue[i - 5])
  brent_pc5[i] <- (finaldata$BrentOilValue[i - 5] - finaldata$BrentOilValue[i - 6])/(finaldata$BrentOilValue[i - 6])
}


# compute price change for jet fuel oil
jetfuel_pc1 = c(); jetfuel_pc2= c(); jetfuel_pc3 = c(); jetfuel_pc4 = c(); jetfuel_pc5 = c();
for (i in 7:nrow(finaldata))
{
  jetfuel_pc1[i] <- (finaldata$JetFuelValue[i - 1] - finaldata$JetFuelValue[i - 2])/(finaldata$JetFuelValue[i - 2])
  jetfuel_pc2[i] <- (finaldata$JetFuelValue[i - 2] - finaldata$JetFuelValue[i - 3])/(finaldata$JetFuelValue[i - 3])
  jetfuel_pc3[i] <- (finaldata$JetFuelValue[i - 3] - finaldata$JetFuelValue[i - 4])/(finaldata$JetFuelValue[i - 4])
  jetfuel_pc4[i] <- (finaldata$JetFuelValue[i - 4] - finaldata$JetFuelValue[i - 5])/(finaldata$JetFuelValue[i - 5])
  jetfuel_pc5[i] <- (finaldata$JetFuelValue[i - 5] - finaldata$JetFuelValue[i - 6])/(finaldata$JetFuelValue[i - 6])
}



# compute price change for Delta adjusted close
delta_pc1 = c(); delta_pc2= c(); delta_pc3 = c(); delta_pc4 = c(); delta_pc5 = c();
for (i in 7:nrow(finaldata))
{
  delta_pc1[i] <- (finaldata$DeltaAdj.Close[i - 1] - finaldata$DeltaAdj.Close[i - 2])/(finaldata$DeltaAdj.Close[i - 2])
}


#created dataset
workdata.withemptyrows <- data.frame(snp_pc1, snp_pc2, snp_pc3, snp_pc4, snp_pc5, brent_pc1, brent_pc2, brent_pc3, brent_pc4, brent_pc5, jetfuel_pc1, jetfuel_pc2, jetfuel_pc3, jetfuel_pc4, jetfuel_pc5, delta_pc1 )
#adding date columns
#workdata$DeltaAdj.Close <- finaldata$DeltaAdj.Close
#workdata$Date <- finaldata$Date
#removing empty rows
workdata <- workdata.withemptyrows[7:nrow(workdata.withemptyrows),]

#partition

workdata$delta_01 <- ifelse(workdata$delta_pc1 >= 0.00033, 1, 0)
rnum <- (runif(1, .60, .70))
#rnum
part <-sample(1:nrow(workdata), rnum * nrow(workdata))
trng <- workdata[part,]
test <- workdata[-part,]

#perform regression
linear.reg <- lm(delta_pc1 ~  ., data = trng)
#summary(linear.reg);
#root mean square error
linear.rmse  <- sqrt(mean(linear.reg$residuals)^2);
#predicting the test data
linear.predict <- predict(linear.reg,test)

####### Logistic Regression ########
avg <- mean(workdata$delta_pc1)

lr.delta <- glm(delta_01 ~ . -delta_pc1, family=binomial(link="logit"), data=trng)
#summary(lr.delta)

#iterations of glm by removing the non significant columns one by one, starting with least significant
lr.delta.01 <- glm(delta_01 ~ snp_pc1 + brent_pc1 + brent_pc2 + brent_pc3 + jetfuel_pc1 + jetfuel_pc3 + jetfuel_pc4, family=binomial(link="logit"), data=trng)
#summary(lr.delta.01)

lr.delta.02 <- glm(delta_01 ~ snp_pc1 + brent_pc1 + brent_pc3 + jetfuel_pc1, family=binomial(link="logit"), data=trng)
#summary(lr.delta.02)

#Let's predict
pred1 <- predict(lr.delta , newdata=test, type="response")
pred2 <- predict(lr.delta.01 , newdata=test, type="response")
pred3 <- predict(lr.delta.02 , newdata=test, type="response")

pred_v <- c(pred1,pred2,pred3)

#converting predictions > 50% to 1 and remaining to 0
pred_1 <- ifelse(pred1 > 0.5,1,0)
pred_2 <- ifelse(pred2 > 0.5,1,0)
pred_3 <- ifelse(pred3 > 0.5,1,0)

#table(test$delta_01,pred_1)
#table(test$delta_01,pred_2)
#table(test$delta_01,pred_3)

#measuring accuracy Accuracy : 0.6752, 0.6907, 0.6874
library("e1071")
library("caret")
#confusionMatrix(test$delta_01, pred_1)
#confusionMatrix(test$delta_01, pred_2)
#confusionMatrix(test$delta_01, pred_3)

# Modelling using Recursive Partition Tree
#install.packages('rpart')
library('rpart')
model.rpt <- rpart(delta_01 ~ snp_pc1 + snp_pc4 + snp_pc5 + brent_pc1 + brent_pc2 + brent_pc4 + jetfuel_pc5 + jetfuel_pc3 + jetfuel_pc1, data=trng, cp=0)
#plot(model.rpt)
#text(model.rpt, use.n= T, digits=3, cex=0.6)
prediction.rpt <- predict(model.rpt, newdata = test, type="vector")
pred_rpt <- ifelse(prediction.rpt > 0.5,1,0)
#printcp(model.rpt)
#table(pred_rpt, test$delta_01)

#accuracy - does not seems to improve - we can try using more combinations of features
#confusionMatrix(test$delta_01, pred_rpt)

# Modelling using Gradient Boosting
#install.packages('gbm')
library('gbm')
model.gbm <- gbm(delta_01 ~ snp_pc1 + brent_pc1 + brent_pc2 + brent_pc3 + jetfuel_pc1 + jetfuel_pc3 + jetfuel_pc4, data=trng , n.trees=5000, interaction.depth =6, shrinkage=0.01)
prediction.gbm <- predict(model.gbm, newdata = test, n.trees=5000, type="response",na.action = na.pass)
#head(prediction.gbm[])
#tail(prediction.gbm[])
#summary(prediction.gbm)

pred_gbm <- ifelse(prediction.gbm > 0.42,1,0)
printcp(model.rpt)
#table(pred_gbm, test$delta_01)
# accuracy remains around 61-62%
#confusionMatrix(pred_gbm, test$delta_01)

workdata.nb <- workdata.withemptyrows
tempsnppccal <- workdata.nb[7:nrow(workdata.nb),]

#imputing NAs from brent pc columns by mean values of that column
workdata.nb[is.na(workdata.nb[,6]), 6] <- mean(workdata.nb[,6], na.rm = TRUE)
workdata.nb[is.na(workdata.nb[,7]), 7] <- mean(workdata.nb[,7], na.rm = TRUE)
workdata.nb[is.na(workdata.nb[,8]), 8] <- mean(workdata.nb[,8], na.rm = TRUE)
workdata.nb[is.na(workdata.nb[,9]), 9] <- mean(workdata.nb[,9], na.rm = TRUE)
workdata.nb[is.na(workdata.nb[,10]), 10] <- mean(workdata.nb[,10], na.rm = TRUE)

# create categorical values for sp500 percentage changes based on std dev.
snp.sd1 = sd(tempsnppccal$snp_pc1)
snp.sd2 = sd(tempsnppccal$snp_pc2)
snp.sd3 = sd(tempsnppccal$snp_pc3)
snp.sd4 = sd(tempsnppccal$snp_pc4)
snp.sd5 = sd(tempsnppccal$snp_pc5)

snp_cat1 = c(); snp_cat2= c(); snp_cat3 = c(); snp_cat4 = c(); snp_cat5 = c()

for (i in 7:nrow(workdata.nb))
{
  
  snp_cat1[i] = ifelse(snp_pc1[i] < -1 * snp.sd1, 'awful', 
                       (ifelse((snp_pc1[i] >= -1 * snp.sd1 && snp_pc1[i] < -0.3 * snp.sd1), 'Bad',
                               (ifelse((snp_pc1[i] >= -0.3 * snp.sd1 && snp_pc1[i] < 0.3 * snp.sd1), 'Unchanged',
                                       (ifelse((snp_pc1[i] >= 0.3 * snp.sd1 && snp_pc1[i] < snp.sd1), 'Good',
                                               (ifelse((snp_pc1[i] >= snp.sd1), 'Great', 'None')))))))))
  snp_cat2[i] = ifelse(snp_pc2[i] < -1 * snp.sd2, 'awful', 
                       (ifelse((snp_pc2[i] >= -1 * snp.sd2 && snp_pc2[i] < -0.3 * snp.sd2), 'Bad',
                               (ifelse((snp_pc2[i] >= -0.3 * snp.sd2 && snp_pc2[i] < 0.3 * snp.sd2), 'Unchanged',
                                       (ifelse((snp_pc2[i] >= 0.3 * snp.sd2 && snp_pc2[i] < snp.sd2), 'Good',
                                               (ifelse((snp_pc2[i] >= snp.sd2), 'Great', 'None')))))))))
  
  snp_cat3[i] = ifelse(snp_pc3[i] < -1 * snp.sd3, 'awful', 
                       (ifelse((snp_pc3[i] >= -1 * snp.sd3 && snp_pc3[i] < -0.3 * snp.sd3), 'Bad',
                               (ifelse((snp_pc3[i] >= -0.3 * snp.sd3 && snp_pc3[i] < 0.3 * snp.sd3), 'Unchanged',
                                       (ifelse((snp_pc3[i] >= 0.3 * snp.sd3 && snp_pc3[i] < snp.sd3), 'Good',
                                               (ifelse((snp_pc3[i] >= snp.sd3), 'Great', 'None')))))))))
  
  snp_cat4[i] = ifelse(snp_pc4[i] < -1 * snp.sd4, 'awful', 
                       (ifelse((snp_pc4[i] >= -1 * snp.sd4 && snp_pc4[i] < -0.3 * snp.sd4), 'Bad',
                               (ifelse((snp_pc4[i] >= -0.3 * snp.sd4 && snp_pc4[i] < 0.3 * snp.sd4), 'Unchanged',
                                       (ifelse((snp_pc4[i] >= 0.3 * snp.sd4 && snp_pc4[i] < snp.sd4), 'Good',
                                               (ifelse((snp_pc4[i] >= snp.sd4), 'Great', 'None')))))))))
  snp_cat5[i] = ifelse(snp_pc5[i] < -1 * snp.sd5, 'awful', 
                       (ifelse((snp_pc5[i] >= -1 * snp.sd5 && snp_pc5[i] < -0.3 * snp.sd5), 'Bad',
                               (ifelse((snp_pc5[i] >= -0.3 * snp.sd5 && snp_pc5[i] < 0.3 * snp.sd5), 'Unchanged',
                                       (ifelse((snp_pc5[i] >= 0.3 * snp.sd5 && snp_pc5[i] < snp.sd5), 'Good',
                                               (ifelse((snp_pc5[i] >= snp.sd5), 'Great', 'None')))))))))
  
  
}




###
###
# create categorical values for brent oil percentage changes based on std dev.
brent.sd1 = sd(tempsnppccal$brent_pc1, na.rm = T)
brent.sd2 = sd(tempsnppccal$brent_pc2, na.rm = T)
brent.sd3 = sd(tempsnppccal$brent_pc3, na.rm = T)
brent.sd4 = sd(tempsnppccal$brent_pc4, na.rm = T)
brent.sd5 = sd(tempsnppccal$brent_pc5, na.rm = T)
brent_cat1 = c(); brent_cat2= c(); brent_cat3 = c(); brent_cat4 = c(); brent_cat5 = c()

for (i in 7:nrow(workdata.nb))
{
  
  brent_cat1[i] = ifelse(brent_pc1[i] < -1 * brent.sd1, 'awful', 
                         (ifelse((brent_pc1[i] >= -1 * brent.sd1 && brent_pc1[i] < -0.3 * brent.sd1), 'Bad',
                                 (ifelse((brent_pc1[i] >= -0.3 * brent.sd1 && brent_pc1[i] < 0.3 * brent.sd1), 'Unchanged',
                                         (ifelse((brent_pc1[i] >= 0.3 * brent.sd1 && brent_pc1[i] < brent.sd1), 'Good',
                                                 (ifelse((brent_pc1[i] >= brent.sd1), 'Great', 'None')))))))))
  brent_cat2[i] = ifelse(brent_pc2[i] < -1 * brent.sd2, 'awful', 
                         (ifelse((brent_pc2[i] >= -1 * brent.sd2 && brent_pc2[i] < -0.3 * brent.sd2), 'Bad',
                                 (ifelse((brent_pc2[i] >= -0.3 * brent.sd2 && brent_pc2[i] < 0.3 * brent.sd2), 'Unchanged',
                                         (ifelse((brent_pc2[i] >= 0.3 * brent.sd2 && brent_pc2[i] < brent.sd2), 'Good',
                                                 (ifelse((brent_pc2[i] >= brent.sd2), 'Great', 'None')))))))))
  
  brent_cat3[i] = ifelse(brent_pc3[i] < -1 * brent.sd3, 'awful', 
                         (ifelse((brent_pc3[i] >= -1 * brent.sd3 && brent_pc3[i] < -0.3 * brent.sd3), 'Bad',
                                 (ifelse((brent_pc3[i] >= -0.3 * brent.sd3 && brent_pc3[i] < 0.3 * brent.sd3), 'Unchanged',
                                         (ifelse((brent_pc3[i] >= 0.3 * brent.sd3 && brent_pc3[i] < brent.sd3), 'Good',
                                                 (ifelse((brent_pc3[i] >= brent.sd3), 'Great', 'None')))))))))
  
  brent_cat4[i] = ifelse(brent_pc4[i] < -1 * brent.sd4, 'awful', 
                         (ifelse((brent_pc4[i] >= -1 * brent.sd4 && brent_pc4[i] < -0.3 * brent.sd4), 'Bad',
                                 (ifelse((brent_pc4[i] >= -0.3 * brent.sd4 && brent_pc4[i] < 0.3 * brent.sd4), 'Unchanged',
                                         (ifelse((brent_pc4[i] >= 0.3 * brent.sd4 && brent_pc4[i] < brent.sd4), 'Good',
                                                 (ifelse((brent_pc4[i] >= brent.sd4), 'Great', 'None')))))))))
  brent_cat5[i] = ifelse(brent_pc5[i] < -1 * brent.sd5, 'awful', 
                         (ifelse((brent_pc5[i] >= -1 * brent.sd5 && brent_pc5[i] < -0.3 * brent.sd5), 'Bad',
                                 (ifelse((brent_pc5[i] >= -0.3 * brent.sd5 && brent_pc5[i] < 0.3 * brent.sd5), 'Unchanged',
                                         (ifelse((brent_pc5[i] >= 0.3 * brent.sd5 && brent_pc5[i] < brent.sd5), 'Good',
                                                 (ifelse((brent_pc5[i] >= brent.sd5), 'Great', 'None')))))))))
  
  
}




###
###
# create categorical values for jetfuel oil percentage changes based on std dev.
jetfuel.sd1 = sd(tempsnppccal$jetfuel_pc1, na.rm = T)
jetfuel.sd2 = sd(tempsnppccal$jetfuel_pc2, na.rm = T)
jetfuel.sd3 = sd(tempsnppccal$jetfuel_pc3, na.rm = T)
jetfuel.sd4 = sd(tempsnppccal$jetfuel_pc4, na.rm = T)
jetfuel.sd5 = sd(tempsnppccal$jetfuel_pc5, na.rm = T)
jetfuel_cat1 = c(); jetfuel_cat2= c(); jetfuel_cat3 = c(); jetfuel_cat4 = c(); jetfuel_cat5 = c()

for (i in 7:nrow(workdata.nb))
{
  
  jetfuel_cat1[i] = ifelse(jetfuel_pc1[i] < -1 * jetfuel.sd1, 'awful', 
                           (ifelse((jetfuel_pc1[i] >= -1 * jetfuel.sd1 && jetfuel_pc1[i] < -0.3 * jetfuel.sd1), 'Bad',
                                   (ifelse((jetfuel_pc1[i] >= -0.3 * jetfuel.sd1 && jetfuel_pc1[i] < 0.3 * jetfuel.sd1), 'Unchanged',
                                           (ifelse((jetfuel_pc1[i] >= 0.3 * jetfuel.sd1 && jetfuel_pc1[i] < jetfuel.sd1), 'Good',
                                                   (ifelse((jetfuel_pc1[i] >= jetfuel.sd1), 'Great', 'None')))))))))
  jetfuel_cat2[i] = ifelse(jetfuel_pc2[i] < -1 * jetfuel.sd2, 'awful', 
                           (ifelse((jetfuel_pc2[i] >= -1 * jetfuel.sd2 && jetfuel_pc2[i] < -0.3 * jetfuel.sd2), 'Bad',
                                   (ifelse((jetfuel_pc2[i] >= -0.3 * jetfuel.sd2 && jetfuel_pc2[i] < 0.3 * jetfuel.sd2), 'Unchanged',
                                           (ifelse((jetfuel_pc2[i] >= 0.3 * jetfuel.sd2 && jetfuel_pc2[i] < jetfuel.sd2), 'Good',
                                                   (ifelse((jetfuel_pc2[i] >= jetfuel.sd2), 'Great', 'None')))))))))
  
  jetfuel_cat3[i] = ifelse(jetfuel_pc3[i] < -1 * jetfuel.sd3, 'awful', 
                           (ifelse((jetfuel_pc3[i] >= -1 * jetfuel.sd3 && jetfuel_pc3[i] < -0.3 * jetfuel.sd3), 'Bad',
                                   (ifelse((jetfuel_pc3[i] >= -0.3 * jetfuel.sd3 && jetfuel_pc3[i] < 0.3 * jetfuel.sd3), 'Unchanged',
                                           (ifelse((jetfuel_pc3[i] >= 0.3 * jetfuel.sd3 && jetfuel_pc3[i] < jetfuel.sd3), 'Good',
                                                   (ifelse((jetfuel_pc3[i] >= jetfuel.sd3), 'Great', 'None')))))))))
  
  jetfuel_cat4[i] = ifelse(jetfuel_pc4[i] < -1 * jetfuel.sd4, 'awful', 
                           (ifelse((jetfuel_pc4[i] >= -1 * jetfuel.sd4 && jetfuel_pc4[i] < -0.3 * jetfuel.sd4), 'Bad',
                                   (ifelse((jetfuel_pc4[i] >= -0.3 * jetfuel.sd4 && jetfuel_pc4[i] < 0.3 * jetfuel.sd4), 'Unchanged',
                                           (ifelse((jetfuel_pc4[i] >= 0.3 * jetfuel.sd4 && jetfuel_pc4[i] < jetfuel.sd4), 'Good',
                                                   (ifelse((jetfuel_pc4[i] >= jetfuel.sd4), 'Great', 'None')))))))))
  jetfuel_cat5[i] = ifelse(jetfuel_pc5[i] < -1 * jetfuel.sd4, 'awful', 
                           (ifelse((jetfuel_pc5[i] >= -1 * jetfuel.sd5 && jetfuel_pc5[i] < -0.3 * jetfuel.sd5), 'Bad',
                                   (ifelse((jetfuel_pc5[i] >= -0.3 * jetfuel.sd5 && jetfuel_pc5[i] < 0.3 * jetfuel.sd5), 'Unchanged',
                                           (ifelse((jetfuel_pc5[i] >= 0.3 * jetfuel.sd5 && jetfuel_pc5[i] < jetfuel.sd5), 'Good',
                                                   (ifelse((jetfuel_pc5[i] >= jetfuel.sd5), 'Great', 'None')))))))))
  
  
}

deltapriceDir = c();
for (i in 7:nrow(workdata.nb))
{
  deltapriceDir[i] <- ifelse((workdata.nb$delta_pc1[i] - workdata.nb$delta_pc1[i - 1]) > 0, 'High', 'Low')
}

work.nbALL <- data.frame(snp_cat1, snp_cat2, snp_cat3, snp_cat4, snp_cat5, brent_cat1, brent_cat2, brent_cat3, brent_cat4, brent_cat5, jetfuel_cat1, jetfuel_cat2, jetfuel_cat3, jetfuel_cat4, jetfuel_cat5, deltapriceDir)

work.nb <- work.nbALL[7:nrow(work.nbALL),]
#replacing 1 NA with Low
work.nb[1,ncol(work.nb)] <- work.nb[2,ncol(work.nb)]
work.nb$deltapriceDir <- as.factor(work.nb$deltapriceDir)

#removingg NA from brent oil columns
work.nb[is.na(work.nb[,6]), 6] <- 'Unchanged'
work.nb[is.na(work.nb[,7]), 7] <- 'Unchanged'
work.nb[is.na(work.nb[,8]), 8] <- 'Unchanged'
work.nb[is.na(work.nb[,9]), 9] <- 'Unchanged'
work.nb[is.na(work.nb[,10]), 10] <- 'Unchanged'
#partition
part.nb <-sample(1:nrow(work.nb), rnum * nrow(work.nb))
trng.nb <- work.nb[part,]
test.nb <- work.nb[-part,]

#Naive Bayes
# Modeling using NaiveBayes #required "caret" package
nb <- train(deltapriceDir ~ . , data=trng.nb, method="nb")
#predict
predNB <- predict(nb, test.nb)

#accuracy measures 
acc<-mean(predNB==test.nb$deltapriceDir) #print acc to get the value
#confusionMatrix(predNB, test.nb$deltapriceDir)

### R CODE ENDS

server <- shinyServer(function(input, output){  
  # Return the requested dataset
  jetfuel <- read.csv(file = 'jetfuelprices.csv', skip = 12, header = T, as.is = T, na.strings = "#N/A")
  delta <- read.csv(file = 'Deltastocks.csv', skip = 0, header = T, as.is = T, na.strings = "NA")
  sp500 <- read.csv(file = 'Proj-SP500.csv', skip = 0, header = T, as.is = T, na.strings = "NA")
  brentoil <- read.csv(file = 'brentoilprices.csv', skip = 12, header = T, as.is = T, na.strings = c("NA", "#N/A" ))
  
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

