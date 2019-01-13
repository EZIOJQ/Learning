
cars = read.csv("cars.csv")
seqx = seq(50,450,50) 
labx = c("50","100","150","200","250","300","350","400","450")
seqy = seq(40,240,40)
laby = c("40","80","120","160","200","240")
with(cars,plot(engine,horse,pch=16,axes=FALSE,xlab="engine",ylab="housepower"))
axis(1,at=seqx,labels=labx)
axis(2,at=seqy,labels=laby)

model = lm(horse~engine,data=cars)
summary(model)
abline(41.001946,0.327309)

x=cor(x = cars$engine,y = cars$horse,use = "complete.obs")

cars$engine2 = cars$engine
cars$engine2[is.na(cars$horse)]=NA
engine2_mean = mean(cars$engine2, na.rm=TRUE)
engine2_sd = sd(cars$engine2, na.rm=TRUE)
horse_mean = mean(cars$horse, na.rm=TRUE)
horse_sd = sd(cars$horse, na.rm=TRUE)

engine2_mean
engine2_sd
horse_mean
horse_sd

model2 = lm(horse~engine2,data=cars)
summary(model2)

x^2
sst = sum((cars$horse - horse_mean)^2)
ssr = sum((cars$horse -( 0.327309*cars$engine2+41.001946)^2))
R2 = (sst-ssr)/sst          
R2          
          
cars$scaled_horse = (cars$horse- horse_mean)/horse_sd
scaled_horse_mean = mean(cars$scaled_horse,na.rm=TRUE)
scaled_horse_sd = sd(cars$scaled_horse,na.rm=TRUE)
cars$scaled_engine = (cars$engine2 - engine2_mean)/engine2_sd
scaled_engine_mean = mean(cars$scaled_engine,na.rm = TRUE)
scaled_engine_sd = sd(cars$scaled_engine,na.rm = TRUE)

scaled_engine_mean
scaled_engine_sd
scaled_horse_mean
scaled_horse_sd
cor(x=cars$scaled_engine,y = cars$scaled_horse,use = "complete.obs" )

model3 = lm(scaled_horse~scaled_engine,data= cars)
summary(model3)


SSR = sum(model$residuals**2, na.rm=TRUE)
s = sqrt(SSR / model$df.residual)

sqrt_sumx2 = with(cars, sqrt(sum((engine2 - engine2_mean)**2, na.rm=TRUE)))
Standarderror = s/sqrt_sumx2
Standarderror

cars$engine == 180
data = data.frame(engine =180)
predict(model, engine = 180)

sd = with ( cars,s*sqrt(1/( model$df.residual+2) + (180- engine2_mean)^2 / sum((engine2 - engine2_mean)^2,na.rm=TRUE)))
sd
model$df.residual 

t = qt(0.95,398)
t
Y = 0.327309*180 + 41.001946   
t*sd
c(Y - t*sd, Y+t*sd)


band = data.frame(engine2=seq(min(cars$engine2, na.rm=TRUE), max(cars$engine2, na.rm=TRUE)))
y=predict(model2, band, interval="confidence", level=0.90)
points(band$engine2, y[,2], pch=".",cex=2, col="blue") #Adds lower limits to scatter plot
points(band$engine2, y[,3], pch=".", cex=2,col="blue") #Adds upper limits to scatter plot
prediction=predict.lm(model2, band, interval="prediction", level=0.90)
points(band$engine2, prediction[,2], pch=".",cex=2, col="green") #Adds lower limits to scatter plot
points(band$engine2, prediction[,3], pch=".", cex=2, col="green") #Adds upper limits to scatter plot



