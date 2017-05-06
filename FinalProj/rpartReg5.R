#recursive partition

library('rpart')
model.rpt <- rpart(delta_01 ~ snp_pc1 + snp_pc4 + snp_pc5 + brent_pc1 + brent_pc2 + brent_pc4 + jetfuel_pc5 + jetfuel_pc3 + jetfuel_pc1, data=trng, cp=0)
plot(model.rpt)
text(model.rpt, use.n= T, digits=3, cex=0.6)
prediction.rpt <- predict(model.rpt, newdata = test, type="vector")
pred_rpt <- ifelse(prediction.rpt > 0.5,1,0)
#printcp(model.rpt)
table(pred_rpt, test$delta_01)

#accuracy - does not seems to improve - we can try using more combinations of features
confusionMatrix(test$delta_01, pred_rpt)