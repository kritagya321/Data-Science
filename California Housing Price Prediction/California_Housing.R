#Kritgaya Nepal
#kxn190007
california <- read.csv("https://personal.utdallas.edu/~kxn190007/housing.csv")
View(california)
dim(california)
summary(california)

summary(california)

#Reminvg null values
cali <- as.data.frame(na.exclude(california))
sum(is.na(california))
dim(cali)

#Histogram
hist(cali$median_house_value)

#Corelation Plot
require(corrplot)
M <- cor(cali[-which(names(cali)=="ocean_proximity")])
corrplot(M, method="circle")

#Corelation of Median House Price with all other features
cor(cali$median_house_value,cali$longitude)
cor(cali$median_house_value,cali$latitude)
cor(cali$median_house_value,cali$housing_median_age)
cor(cali$median_house_value,cali$total_rooms)
cor(cali$median_house_value,cali$total_bedrooms)
cor(cali$median_house_value,cali$population)
cor(cali$median_house_value,cali$households)
cor(cali$median_house_value,cali$median_income)

#Plot of meidan house vs median income
plot(cali$median_income,cali$median_house_value,xlab = "Median Income", ylab = "House Value", col = "dark red")
plot(cali$population,cali$median_house_value, xlab = "Populatioin", ylab = "House Value",col = "dark green")
plot(cali$total_bedrooms,cali$median_house_value,xlab = "Total Bedrooms", ylab = "House Value", col = "light blue")
plot(cali$households,cali$median_house_value,xlab = "Household", ylab = "House Value", col = "pink")

#linear model
model1 <- lm(cali$median_house_value~cali$longitude,cali)
model2 <- lm(cali$median_house_value~cali$latitude,cali)
model3 <- lm(cali$median_house_value~cali$housing_median_age,cali)
model4 <- lm(cali$median_house_value~cali$total_rooms)
model5 <- lm(cali$median_house_value~cali$population,cali)
model6 <- lm(cali$median_house_value~cali$households,cali)
model7 <- lm(cali$median_house_value~cali$median_income,cali)
model8 <- lm(cali$median_house_value~cali$total_bedrooms)


#Summary od each individual model.
summary(model1)
summary(model2)
summary(model3)
summary(model4)
summary(model5)
summary(model6)
summary(model7)
summary(model8)

#Model with all features except the ocean_proximity
model9 <- lm(cali$median_house_value~.,cali)
summary(model9)

#Model with all but derived features
model10 <- lm(cali$median_house_value~cali$longitude+cali$latitude+cali$total_rooms+cali$population+cali$median_income
              +cali$housing_median_age, cali)
summary(model10)


