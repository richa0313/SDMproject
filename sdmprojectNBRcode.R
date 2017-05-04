##### NAIVE BAYES #####

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
  
  brent_cat1[i] = ifelse(brent_pc1[i] < -1 * brent.sd, 'awful', 
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

#work.nb[is.na(work.nb[,6]), 6] <- mean(work.nb[,6], na.rm = TRUE)

#partition
part.nb <-sample(1:nrow(work.nb), rnum * nrow(work.nb))
trng.nb <- work.nb[part,]
test.nb <- work.nb[-part,]

#Naive Bayes
# Modeling using NaiveBayes
model.NB <- NaiveBayes(deltapriceDir ~ . , data = trng.nb)
test.nb <- test.nb[complete.cases(trng.nb),]
predict.nb <- predict(model.NB, data = test.nb)
table(predict.nb, test$delta_pc1)
confusionMatrix(test.nb$deltapriceDir, predict.nb$class)


