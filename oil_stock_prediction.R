setwd("F:/Study/SDM")

#reading file and setting the junks values to NA
jetfuel <- read.csv(file = 'jetfuelprices.csv', skip = 12, header = T, as.is = T, na.strings = "#N/A")
delta <- read.csv(file = 'Deltastocks.csv', skip = 0, header = T, as.is = T, na.strings = "NA")
sp500 <- read.csv(file = 'Proj-SP500.csv', skip = 0, header = T, as.is = T, na.strings = "NA")
brentoil <- read.csv(file = 'brentoilprices.csv', skip = 12, header = T, as.is = T, na.strings = "#N/A")

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
workdata <- data.frame(snp_pc1, snp_pc2, snp_pc3, snp_pc4, snp_pc5, brent_pc1, brent_pc2, brent_pc3, brent_pc4, brent_pc5, jetfuel_pc1, jetfuel_pc2, jetfuel_pc3, jetfuel_pc4, jetfuel_pc5, delta_pc1 )
#adding date columns
#workdata$DeltaAdj.Close <- finaldata$DeltaAdj.Close
#workdata$Date <- finaldata$Date
#removing empty rows
workdata <- workdata[7:nrow(workdata),]

#partition
rnum <- (runif(1, .60, .70))
rnum
part <-sample(1:nrow(workdata), rnum * nrow(workdata))
trng <- workdata[part,]
test <- workdata[-part,]

#perform regression
linear.reg <- lm(delta_pc1 ~  ., data = trng)
summary(linear.reg);
#root mean square error
linear.rmse  <- sqrt(mean(linear.reg$residuals)^2);
#predicting the test data
linear.predict <- predict(linear.reg,test)


####### Logistic Regression ########

avg <- mean(workdata$delta_pc1)
workdata$delta_01 <- ifelse(workdata$delta_pc1 >= 0.00033, 1, 0)

lr.delta <- glm(delta_01 ~ . -delta_pc1, family=binomial(link="logit"), data=trng)
summary(lr.delta)

#iterations of glm by removing the non significant columns one by one, starting with least significant
lr.delta.01 <- glm(delta_01 ~ snp_pc1 + brent_pc1 + brent_pc2 + brent_pc3 + jetfuel_pc1 + jetfuel_pc3 + jetfuel_pc4, family=binomial(link="logit"), data=trng)
summary(lr.delta.01)

lr.delta.02 <- glm(delta_01 ~ snp_pc1 + brent_pc1 + brent_pc3 + jetfuel_pc1, family=binomial(link="logit"), data=trng)
summary(lr.delta.02)

#Let's predict
pred1 <- predict(lr.delta , newdata=test, type="response")
pred2 <- predict(lr.delta.01 , newdata=test, type="response")
pred3 <- predict(lr.delta.02 , newdata=test, type="response")

pred_v <- c(pred1,pred2,pred3)

#converting predictions > 50% to 1 and remaining to 0
pred_1 <- ifelse(pred1 > 0.5,1,0)
pred_2 <- ifelse(pred2 > 0.5,1,0)
pred_3 <- ifelse(pred3 > 0.5,1,0)

table(test$delta_01,pred_1)
table(test$delta_01,pred_2)
table(test$delta_01,pred_3)

#measuring accuracy Accuracy : 0.6752, 0.6907, 0.6874
confusionMatrix(test$delta_01, pred_1)
confusionMatrix(test$delta_01, pred_2)
confusionMatrix(test$delta_01, pred_3)





















