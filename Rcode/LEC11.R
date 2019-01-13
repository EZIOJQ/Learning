cars = read.csv("cars.csv")
library("car")
pairs(~mpg + engine+horse + weight + accel, data=cars,pch=16)
model = lm(mpg~engine+horse + weight + accel, data = cars)
summary(model)
newdata = data.frame(engine= 200,horse=100,weight=3000,accel=15)
predict(model, newdata,interval = "prediction", level = .9)
with(model,plot(fitted.values,residuals,pch=16))
abline(0,0)
cor(cars[,c("mpg","engine","horse","weight","accel")],use="complete.obs")
vif(model)

enginesize= model$model$engine
model2 = lm(mpg~engine, data = cars)

plot(x= model$model$engine, y = residuals(model),xlab= "engine size", ylab="residuals",pch=16)
abline(h  =0)

model3 = lm(mpg~weight, data=cars)
summary(model3)

complete = subset(cars, !is.na(mpg) & !is.na(weight) & !is.na(horse) & !is.na(engine) & !is.na(accel))
mpg_model = lm(mpg ~ engine + horse + weight + accel, data = complete)
mpg_model2 = lm(mpg ~ weight, data = complete)
anova(mpg_model,mpg_model2)

model4 = lm(mpg~weight+accel, data =complete)
summary(model4)
anova(mpg_model2, model4)

model5 = lm(mpg~weight+horse, data = complete)
summary(model5)
anova(model4, model5)






