####### Logistic Regression ########
library("e1071")
library("caret")

lr.delta <- glm(delta_01 ~ . -delta_pc1, family=binomial(link="logit"), data=trng)
summary(lr.delta)

#iterations of glm by removing the non significant columns one by one, starting with least significant
lr.delta.01 <- glm(delta_01 ~ snp_pc1 + brent_pc1 + brent_pc2 + brent_pc3 + jetfuel_pc1 + jetfuel_pc3 + jetfuel_pc4, family=binomial(link="logit"), data=trng)
summary(lr.delta.01)

lr.delta.02 <- glm(delta_01 ~ snp_pc1 + brent_pc1 + brent_pc3 + jetfuel_pc1, family=binomial(link="logit"), data=trng)
summary(lr.delta.02)

#Let's predict
pred1 <- predict(lr.delta , newdata=test, type="response")
pred2 <- predict(lr.delta.01 , newdata=test, type="response")
pred3 <- predict(lr.delta.02 , newdata=test, type="response")

pred_v <- c(pred1,pred2,pred3)

#converting predictions > 50% to 1 and remaining to 0
pred_1 <- ifelse(pred1 > 0.5,1,0)
pred_2 <- ifelse(pred2 > 0.5,1,0)
pred_3 <- ifelse(pred3 > 0.5,1,0)

table(test$delta_01,pred_1)
table(test$delta_01,pred_2)
table(test$delta_01,pred_3)

#measuring accuracy Accuracy : 0.6752, 0.6907, 0.6874
confusionMatrix(test$delta_01, pred_1)
confusionMatrix(test$delta_01, pred_2)
confusionMatrix(test$delta_01, pred_3)