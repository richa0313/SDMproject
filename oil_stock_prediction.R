setwd("F:/Study/SDM")

#reading file and setting the junks values to NAs
jetfuel <- read.csv(file = 'jetfuelprices.csv', skip = 12, header = T, as.is = T, na.strings = "#N/A")
delta <- read.csv(file = 'Deltastocks.csv', skip = 0, header = T, as.is = T, na.strings = "NA")
sp500 <- read.csv(file = 'Proj-SP500.csv', skip = 0, header = T, as.is = T, na.strings = "NA")
brentoil <- read.csv(file = 'brentoilprices.csv', skip = 12, header = T, as.is = T, na.strings = "#N/A")

#checking the NA values
colSums(is.na(jetfuel))
colSums(is.na(delta))
colSums(is.na(sp500))
colSums(is.na(brentoil))

#setting up the dates format and extracting/renaming required columns
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

#removing redundant date columns 3, 5 and 7 one by one
finaldata[3] <- NULL
#write.csv(finaldata, "FinalData.csv")
#plot(finaldata$sp500Adj.Close, finaldata$DeltaAdj.Close)

























