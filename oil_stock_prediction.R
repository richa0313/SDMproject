library(sqldf)
library(mice)

setwd('/Users/birupakhya/Documents/Projects/SDMproject/')
delta = read.csv('Deltastocks.csv', skip = 0, header = T, as.is = T, na.strings = "NA")
jetfuelprices = read.csv('jetfuelprices.csv', skip = 12, header = T, as.is = T, na.strings = "#N/A")
brentprices = read.csv('DCOILBRENTEU.csv',skip = 0, header = T, as.is = T, na.strings = ".")

# Transform variables into correct types
delta$Date = as.Date(delta$Date, "%Y-%m-%d")
jetfuelprices$DATE = as.Date(jetfuelprices$DATE, "%Y-%m-%d")
brentprices$DATE = as.Date(brentprices$DATE, "%Y-%m-%d")

# check missing values
colSums(is.na(delta))
colSums(is.na(jetfuelprices))
colSums(is.na(brentprices))

# check if there is a pattern in the missing data
md.pattern(jetfuelprices)

