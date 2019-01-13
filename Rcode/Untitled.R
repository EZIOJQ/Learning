cars = read.csv("cars.csv")
carsmodel = lm(mpg~engine+horse, data = cars)
plot(carsmodel$fitted.values, carsmodel$resid,xlab="Fitted values", ylab="Residuals", pch=16)
abline(0,0)
summary(carsmodel)
carmodel2 = lm(sqrt(mpg)~engine+ horse, data = cars)
summary(carmodel2)


newdata = data.frame(engine = 150, horse = 100)
mpgS = predict(carmodel2, newdata = newdata, interval = "confidence")
mpgS^2


plot(carmodel2$fitted.values, carmodel2$resid,xlab="Fitted values", ylab="Residuals", pch=16)
abline(0,0)

carmodel3 = lm(log(mpg)~horse+engine, data = cars)
summary(carmodel3)

mpgL = predict(carmodel3, newdata = newdata, interval = "confidence")
10 ^ mpgL

plot(carmodel3$fitted.values, carmodel3$resid,xlab="Fitted values", ylab="Residuals", pch=16)
abline(0,0)


model = lm(log(mpg)~log(horse) + engine, data = cars)
summary(model)

plot(model$fitted.values, model$resid,xlab="Fitted values", ylab="Residuals", pch=16)
abline(0,0)
