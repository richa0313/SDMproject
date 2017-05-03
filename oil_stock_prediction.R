library(sqldf)

setwd('/Users/birupakhya/Documents/Projects/SDMproject/')
delta = read.csv('Deltastocks.csv', skip = 0, header = T, as.is = T, na.strings = "NA")
jetfuelprices = read.csv('jetfuelprices.csv', skip = 12, header = T, as.is = T, na.strings = "#N/A")
brentprices = read.csv('DCOILBRENTEU.csv',skip = 0, header = T, as.is = T, na.strings = ".")

#check missing values
colSums(is.na(delta))
colSums(is.na(jetfuelprices))
colSums(is.na(brentprices))


