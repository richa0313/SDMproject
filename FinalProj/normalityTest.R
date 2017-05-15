## TESTING THE NORMALITY OF THE DATA ##

# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
# usage
libs <- c('httr', 'timeSeries', 'graphics', 'sqldf', 'zoo', 'xts', 'XML', 'lubridate', 'jsonlite', 'reshape2', 'xml2', 'stringr', 'readr', 'quantmod', 'nortest', 'pdfetch')
ipak(libs)

# READ AND CONVERT XTS, ZOO OBJECTS TO DATAFRAME
jfdata <- pdfetch_FRED('DJFUELUSGULF')
jfdata <- data.frame(value=coredata(jfdata),timestamp=index(jfdata))

brentdata <- pdfetch_FRED('DCOILBRENTEU')
brentdata <- data.frame(value=coredata(brentdata),timestamp=index(brentdata))

#identifiers <- c('DAL', '^GSPC')
deldata <- pdfetch_YAHOO('DAL', fields = c("open", "high", "low", "close", "volume", "adjclose"), from = as.Date("1990-04-30"), to = Sys.Date())
deldata <- data.frame(value=coredata(deldata),timestamp=index(deldata))

spdata <- pdfetch_YAHOO('^GSPC', fields = c("open", "high", "low", "close", "volume", "adjclose"), from = as.Date("1990-04-30"), to = Sys.Date())
spdata <- data.frame(value=coredata(spdata),timestamp=index(spdata))


#checking the NA values and omitting it
colSums(is.na(jfdata))
jfdata <- na.omit(jfdata)

colSums(is.na(brentdata))
brentdata <- na.omit(brentdata)

colSums(is.na(spdata))
colSums(is.na(deldata))

#setting up the dates format and extracting required columns
spdata <- spdata[,c(6,7)]
colnames(spdata)[1] <- 'sp500Adj.Close'
colnames(spdata)[2] <- 'Date'

deldata <- deldata[,c(6,7)]
colnames(deldata)[1] <- 'DelAdj.Close'
colnames(deldata)[2] <- 'Date'

colnames(brentdata)[1] <- 'BrentOilValue'
colnames(brentdata)[2] <- 'Date'

colnames(jfdata)[1] <- 'JetFuelValue'
colnames(jfdata)[2] <- 'Date'

#converting the NAs to mean values of the column - Imputation
jfdata[is.na(jfdata[,1]), 1] <- mean(jfdata[,1], na.rm = TRUE)
brentdata[is.na(brentdata[,1]), 1] <- mean(brentdata[,1], na.rm = TRUE)

finaldata <- sqldf('SELECT *
                   FROM deldata 
                   INNER JOIN spdata ON (deldata.Date = spdata.Date)
                   INNER JOIN brentdata ON (deldata.Date = brentdata.DATE)
                   INNER JOIN jfdata ON (deldata.Date = jfdata.DATE)')

#removing redundant date columns 3, 5 and 7
finaldata[2] <- NULL
finaldata[3] <- NULL
finaldata[4] <- NULL

#sorting as per date
finaldata <- finaldata[order(finaldata$Date,decreasing=T),]

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
  delta_pc1[i] <- (finaldata$DelAdj.Close[i - 1] - finaldata$DelAdj.Close[i - 2])/(finaldata$DelAdj.Close[i - 2])
}

#adding ID
#finaldata$ID <- rownames(finaldata)

#created dataset
workdata.withemptyrows <- data.frame(snp_pc1, snp_pc2, snp_pc3, snp_pc4, snp_pc5, brent_pc1, brent_pc2, brent_pc3, brent_pc4, brent_pc5, jetfuel_pc1, jetfuel_pc2, jetfuel_pc3, jetfuel_pc4, jetfuel_pc5, delta_pc1)
#removing empty rows
workdata <- workdata.withemptyrows[7:nrow(workdata.withemptyrows),]

#plotting
ggplot(finaldata, aes(Date, BrentOilValue)) + geom_line() + xlab("") + ylab("BrentOilValue")

#some time series plotting
JetFuelValue <- ts(finaldata[,c('JetFuelValue')])
BrentOilValue <- ts(finaldata[,c('BrentOilValue')]) 
DelAdj.Close <- ts(finaldata[,c('DelAdj.Close')])
sp500Adj.Close <- ts(finaldata[,c('sp500Adj.Close')])

ts.plot(JetFuelValue, col = 'red' , main="ts.plot(x,y)")
ts.plot(BrentOilValue, col = 'blue' , main="ts.plot(x,y)")
ts.plot(DelAdj.Close, col = 'orange' , main="ts.plot(x,y)")
ts.plot(sp500Adj.Close, col = 'green' , main="ts.plot(x,y)")

### Testing normality using qqnorm, qqplot and Shapiro-Wilk test ###

clist <- list('JetFuelValue', 'BrentOilValue', 'DelAdj.Close', 'sp500Adj.Close')

qqgraphs <- function(x) {
	qqnorm(x)
	qqline(x, col = "red")
}

#calling function
qqgraphs(JetFuelValue)
qqgraphs(BrentOilValue)
qqgraphs(DelAdj.Close)
qqgraphs(sp500Adj.Close)

#testing normality using Shapiro-Wilk test and Anderson-Darling test
normtest <- function(x){
  shapiro.test(x)
}
#
ADtest <- function(x){
  ad.test(x)
}

#calling function
normtest(JetFuelValue)
normtest(BrentOilValue)
normtest(DelAdj.Close)
normtest(sp500Adj.Close)

ADtest(JetFuelValue)
ADtest(BrentOilValue)
ADtest(DelAdj.Close)
ADtest(sp500Adj.Close)
######  distribution of variables is not normal

#correlation between everything
c <- cor(workdata)
## c is the correlations matrix

## keep only the lower triangle by filling upper with NA
c[upper.tri(c, diag=TRUE)] <- NA
m <- melt(c)

## sort by descending absolute correlation
m <- m[order(- abs(m$value)), ]

## omit the NA values
dfOut <- na.omit(m)

#correlation between y and X's
x <-  workdata[1:15]
y <- workdata[16]
c <- cor(x, y)
c[order(c)]

rnum <- (runif(1, .60, .70))
#rnum
part <-sample(1:nrow(workdata), rnum * nrow(workdata))
trng <- workdata[part,]
test <- workdata[-part,]

# Linear regression
linear.reg <- lm(delta_pc1 ~  ., data = trng)
summary(linear.reg)
#normtest(linear.reg$residuals)
#ADtest(linear.reg$residuals)
#hist(linear.reg$residuals)






