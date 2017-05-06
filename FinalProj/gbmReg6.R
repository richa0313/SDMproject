# Modelling using Gradient Boosting
#install.packages('gbm')
library('gbm')
model.gbm <- gbm(delta_01 ~ snp_pc1 + brent_pc1 + brent_pc2 + brent_pc3 + jetfuel_pc1 + jetfuel_pc3 + jetfuel_pc4, data=trng , n.trees=5000, interaction.depth =6, shrinkage=0.01)
prediction.gbm <- predict(model.gbm, newdata = test, n.trees=5000, type="response",na.action = na.pass)
head(prediction.gbm[])
#tail(prediction.gbm[])
#summary(prediction.gbm)

pred_gbm <- ifelse(prediction.gbm > 0.42,1,0)
#printcp(model.rpt)
#table(pred_gbm, test$delta_01)
# accuracy remains around 61-62%
confusionMatrix(pred_gbm, test$delta_01)