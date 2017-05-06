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
#selecting threshold greater than avg 
#avg <- mean(workdata$delta_pc1)
workdata$delta_01 <- ifelse(workdata$delta_pc1 >= 0.00034, 1, 0)
rnum <- (runif(1, .60, .70))
#rnum
part <-sample(1:nrow(workdata), rnum * nrow(workdata))
trng <- workdata[part,]
test <- workdata[-part,]