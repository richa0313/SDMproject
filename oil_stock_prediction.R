library(sqldf)

delta = read.csv('/Users/birupakhya/Documents/Projects/SDMproject/Deltastocks.csv', skip = 0, header = T, as.is = T, na.strings = "NA")
jetfuelprices = read.csv('/Users/birupakhya/Documents/Projects/SDMproject/jetfuelprices.csv', skip = 12, header = T, as.is = T, na.strings = "#N/A")
brentprices = read.csv('/Users/birupakhya/Documents/Projects/SDMproject/DCOILBRENTEU.csv',skip = 0, header = T, as.is = T, na.strings = ".")

#check missing values
colSums(is.na(delta))
colSums(is.na(jetfuelprices))
colSums(is.na(brentprices))


