#Question1

setwd("C:/Users/Arvind/Desktop/Intermediate Analytics/Week 1")

df <- read.csv('assignment1.csv')

View(df)

summary(df)

sapply(df, sd, na.rm=TRUE)


plot(df, col='green')

#mean, median, standard deviation, min, max, Q1, and Q3 for variable sepal.width

summary(df$Sepal.Width)
#standard Deviation
std <- sd(df$Sepal.Width)
std

plot(df$Sepal.Width)
hist(df$Sepal.Width, main = "Histogram for Sepal.Width", xlab = "Sepal.Width")


#Question 2
#read CSV file
Height_Weight_by_Gender <- read.csv('height_weight_by_gender.csv')

#view data from CSV file
View(Height_Weight_by_Gender)

#view the column names
names(Height_Weight_by_Gender)

# Rename a column in R
colnames(Height_Weight_by_Gender)[colnames(Height_Weight_by_Gender)=="ï..Gender"] <- "Gender"

colnames(Height_Weight_by_Gender)[colnames(Height_Weight_by_Gender)=="Height..inches."] <- "Height(Inches)"

colnames(Height_Weight_by_Gender)[colnames(Height_Weight_by_Gender)=="Weight..lbs."] <- "Weight(lbs)"

#Chcek the renamed columns
names(Height_Weight_by_Gender)

#Cleaning the data

Height_Weight_by_Gender[Height_Weight_by_Gender$`Weight(lbs)`<0,]
count(Height_Weight_by_Gender[Height_Weight_by_Gender$`Weight(lbs)`<0,])

Height_Weight_by_Gender$`Weight(lbs)`[Height_Weight_by_Gender$`Weight(lbs)`<0]= median(Height_Weight_by_Gender$`Weight(lbs)`)


Height_Weight_by_Gender$`Weight(lbs)`[Height_Weight_by_Gender$`Weight(lbs)`<50]=median(Height_Weight_by_Gender$`Weight(lbs)`)



#Intsall packages ggplot2 and colorspace
install.packages("ggplot2")
install.packages("colorspace")
#load the packages
library(ggplot2)
library(colorspace)

#Histogram for men's and women's height
ggplot(Height_Weight_by_Gender, aes(x=Height_Weight_by_Gender$`Height(Inches)`))+
  geom_histogram(aes(shape=factor(Gender), fill=Gender))+
  ggtitle("Histogram of men's and women's height")+
  xlab("Height in Inches")+
  ylab("Count")

#Histogram for men's and women's weight
ggplot(Height_Weight_by_Gender, aes(x=Height_Weight_by_Gender$`Weight(lbs)`))+
  geom_histogram(aes(shape=factor(Gender), fill=Gender))+
  ggtitle("Histogram of men's and women's weight")+
  xlab("Weight in lbs")+
  ylab("Count")

#Scatterplot

ggplot(Height_Weight_by_Gender, aes(x=`Weight(lbs)`, y=`Height(Inches)`))+
  geom_point(aes(shape=factor(Gender),color=factor(Gender)))+
  labs(title="Scatterplot for comparing the weight and height of men and women",
       x="Weight", y="Height")

#Scatterplot2

plot(Height_Weight_by_Gender$`Height(Inches)`[Height_Weight_by_Gender$Gender=="Male"], 
     Height_Weight_by_Gender$`Weight(lbs)`[Height_Weight_by_Gender$Gender=="Male"],
     xlab="Height(inches)",ylab="Weight(lbs)",main="Scatterplot of Height and Weight for Men",col=c("red", "blue"))
legend("topright", cex=0.8, pch=1, pt.cex = 0.5, legend=c("Height","Weight"), lwd=c(2,2), col=c("blue","red"))







