sales = read.csv("package.csv")
t.test(sales$Pack1,sales$Pack2,paired=TRUE)
t.test(sales$Pack1,sales$Pack2)

pizzasales = read.csv("pizzasales.csv")
pizzasales$Competitor = factor(pizzasales$Competitor)
levels(pizzasales$Competitor) = c("No","Yes")
boxplot(Sales ~ Competitor, data=pizzasales, xlab="Competitor within 0.5 Miles", ylab="Average sales per day")


rep1 = subset(pizzasales, Competitor == "No", select = c(Sales, Competitor))
rep1$Competitor =0
rep2 = subset(pizzasales, Competitor == "Yes",select = c(Sales,Competitor))
rep2$Competitor = 1
combined = rbind(rep1,rep2)
model = lm(Sales~Competitor, data = combined)
summary(model)
confint(model,level = 0.95)

rep3 = data.frame(rep1$Sales,rep2$Sales)

model2 = lm(pizzasales$Sales~pizzasales$Income)
summary(model2)

#Colors based on Competitor variable
#The notation c("red","blue")[Competitor] assigns
#Values of red and blue to each store based on whether
#there is no competition (red) or competition (blue).
with(pizzasales,plot(Income,Sales,col=c("red","blue")[Competitor],pch=16))
#Puts a legend in the upper left hand corner
legend(x="topleft",legend=levels(pizzasales$Competitor),col=c("red","blue"),pch=16)
#Creates regression models for no competitors and competitors.
model1=lm(Sales~Income, Competitor=="No",data=pizzasales)
model2=lm(Sales~Income, Competitor=="Yes",data=pizzasales)
#Adds regression lines to scatter plot
abline(model1,col="red",lwd=2)
abline(model2,col="blue",lwd=2)


model3=lm(Sales ~ Income + Competitor + Competitor*Income ,data=pizzasales)
summary(model3)
pizzasales$Competitor =(relevel(factor(pizzasales$Competitor),ref="Yes"))
model3=lm(Sales ~ Income + Competitor + Competitor*Income ,data=pizzasales)
summary(model3)
confint(model3,level =0.90)
