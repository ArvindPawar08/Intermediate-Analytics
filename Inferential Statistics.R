install.packages("MASS")
library(MASS)
Chem_Dataset<-MASS::chem
mean(Chem_Dataset)
t.test(Chem_Dataset,mu=1,alternative = "greater")

i=c(1:24)
print (i)
for(Chem_Dataset in i)+
  if(sd[Chem_Dataset]>1)+
  {print("True")}


Cats_Dataset<-MASS::cats
male <- subset(cats,cats$Sex=="M") 
male
sum(male$Bwt)
mean(male$Bwt)

Female<- subset(cats,cats$Sex=="F")
sum(Female$Bwt)
mean(Female$Bwt)

t.test(male$Bwt,Female$Bwt,var.equal=FALSE)

install.packages("corrplot")
library(corrplot)
Car_dataset<-cor(mtcars)
corrplot(Car_dataset, method = "number") ##postive are in blue and negative are in red
data<-mtcars
cor.test(x=data$wt,y=data$mpg) ##test for correlation

##scaterplot
attach(mtcars)
plot(wt, mpg, main="Weight vs Miles", 
     xlab="Car Weight ", ylab="Miles Per Gallon ", pch=10)
abline(lm(mpg~wt), col="red") # regression line (y~x) 


A<-lm(formula = data$mpg ~ data$wt, data = data) ###Summarize your main analytic findings (e.g., parameter estimate, SE, t-test, and p-value, R-squared, the F-statistic, and residual standard error
summary(A)

##part 3
h <- MASS::hills

summary(h)

c<-cor(hills)

corrplot(c, method="number")

m<-lm(dist~time, data = hills)

scatter.smooth(hills$dist~hills$time)

summary(m)

t.test(hills$dist, alternative = "greater", mu=2)









