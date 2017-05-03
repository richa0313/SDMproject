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


#created dataset
workdata <- data.frame(snp_pc1, snp_pc2, snp_pc3, snp_pc4, snp_pc5, brent_pc1, brent_pc2, brent_pc3, brent_pc4, brent_pc5, jetfuel_pc1, jetfuel_pc2, jetfuel_pc3, jetfuel_pc4, jetfuel_pc5 )
#adding date column
workdata$Date <- finaldata$Date
#removing empty rows
workdata <- workdata[7:nrow(workdata),]
























