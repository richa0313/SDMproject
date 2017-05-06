#perform regression
linear.reg <- lm(delta_pc1 ~  ., data = trng)
summary(linear.reg);
#root mean square error
linear.rmse  <- sqrt(mean(linear.reg$residuals)^2);
#predicting the test data
linear.predict <- predict(linear.reg,test)
#plotting
plot(linear.predict ,test$delta_pc1,xlab="predicted", ylab="actual")
abline(a=0, b=1)
