salary = read.csv("salary.csv")
meanman = mean(salary$male==1)
meanwoman = mean(salary$male==0)
mansd = sd(salary$male==1)
womansd = sd(salary$male==0)



model = lm(salary~ male, data = salary)
summary(model)

model2 = lm(salary~ yearsbg+ male, data = salary )

summary(model2)
confint(model2 ,level = .999)

newdata = data.frame(male= c(1,0), yearsbg = c(20, 20))
newdata
model3 = lm(salary~male+ yearsbg+ male*yearsbg, data = salary)
predict(model3, newdata, level =.9, interval ="confidence")
summary(model3)

model4 = lm(salary~male+yearsbg+market+factor(rank),data= salary)
summary(model4)
