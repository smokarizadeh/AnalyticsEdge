# VIDEO 4
#rm(list=ls(all=TRUE))

setwd("../assign2/")
# Read in data
wine = read.csv("wine.csv")
str(wine)
summary(wine)

# Linear Regression (one variable)
model1 = lm(Price ~ AGST, data=wine)
summary(model1)

#Error is residual, squared fit
#Adjusted R is relative to number of independent variables and number of observations included. 
#So if you add more avraibles that do not help, then adjusted R decreases. in this way you would known if a variable should be included
# Sum of Squared Errors
model1$residuals
#Sum of squared erros
SSE = sum(model1$residuals^2)  
SSE

# Linear Regression (two variables)
model2 = lm(Price ~ AGST + HarvestRain, data=wine)
summary(model2)

#you could see that R squared and adjusted R squared have increased which means improvements
# Sum of Squared Errors
SSE = sum(model2$residuals^2)
SSE

# comparing SSE of two model shows that second model is better, because it is smaller
# Linear Regression (all variables)
model3 = lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data=wine)
summary(model3)

# Sum of Squared Errors
SSE = sum(model3$residuals^2)
SSE

# we can see R~2 is increased, which means improvemnts, SSE is also decreased which means improvements


#quize
modelq = lm (Price ~ HarvestRain + WinterRain, data=wine)
summary(modelq)


# VIDEO 5
#in the summary of lm model, we interpret the results as followings
# std.Error ; how much coefficient could vary from estimate value
# t -value = estimate / std. error, the larger is t-value, the more likely that coefficient would be significant
# Pr(> t) how probabl it is that coefficient would be zero, the smaller the value then the less likely the coefficient would be zero


# Remove FrancePop
model4 = lm(Price ~ AGST + HarvestRain + WinterRain + Age, data=wine)
summary(model4)


# VIDEO 6

# Correlations
cor(wine$WinterRain, wine$Price)
cor(wine$Age, wine$FrancePop)
cor(wine)

# Remove Age and FrancePop
model5 = lm(Price ~ AGST + HarvestRain + WinterRain, data=wine)
summary(model5)


# VIDEO 7

# Read in test set
wineTest = read.csv("wine_test.csv")
str(wineTest)

# Make test set predictions
predictTest = predict(model4, newdata=wineTest)
predictTest

# Compute R-squared
SSE = sum((wineTest$Price - predictTest)^2)
SST = sum((wineTest$Price - mean(wine$Price))^2)
1 - SSE/SST

