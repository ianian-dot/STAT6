## STAT0030 Lab 4
setwd("/Users/iantan/OneDrive - University College London/Postgrad/Masters/MSc DS/Modules /STAT0030/")
library(ISLR)
library(tidyverse)

## EXPLORING LINEAR MODELS 

## Easy SLR: cars dataset
plot(cars) ## linearity check ok
lm1 <- lm(dist ~ speed, cars)
plot(lm1) ## residual check -- checks for linearity, normality, mean 0 error, homoskedasticity, cooks distance, 

## How could we improve the model?
drop1(lm1)

mtcars

## Play around with more regressors: MLR
lm2 <- lm(mpg ~ ., mtcars)
summary(lm2) # check R2 adjusted, check F test for significance of whole model

## --- drop 1 variable 
drop1(lm2) # drop all possible single terms -- then calculate the changes in fit
# note that if AIC is higher -- then fit became worse

plot(mpg ~ wt, mtcars) # very linear -- hence was a bad choice removing! 
pairs(mtcars)

## matrix form 
x = as.matrix(mtcars[,2:ncol(mtcars)])
y = as.matrix(mtcars[,(1)])

## matrix multiplication 
beta_hat <- solve(t(x) %*% x) %*% t(x) %*% y ## solve() gives the inverse of matrix
fitted_ys <- x %*% beta_hat 

## --- Example practice : Birthweights from ISLR 
help("birthwt")
bw_data <- birthwt ## contains birthweights and gestational ages estimated 
bw_data %>% select(age, bwt, )

# we need the dataset from the ucl page instead 
weight <- read.table("birthwt.dat", col.names = c("age","bwt", "isfemale"),
                     header = F)
head(weight)
pairs(weight)

## -- Run regression on age and gender
gfit2 <- lm(bwt ~ ., x = T, data = weight)

summary(gfit2) ## higher weight with age, higher weight for males
anova(gfit2) ## most of variation in weight comes from age rather than gender 

# Results
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1610.28     786.08  -2.049   0.0532 .  
# age           120.89      20.46   5.908 7.28e-06 ***
# isfemale     -163.04      72.81  -2.239   0.0361 *  

## Interpretation
## Intercept = average male weight when age = 0
## Is.female = change in weight for female vs male keeping age constant
## age = change in weight when gestational age increase by 1 unit, for fixed gender 

## -- Alternative representation -- for reference category for sex 
malefemale <- list(male = 1 - weight$isfemale, female = weight$isfemale) ##indicator variable for both male and female (collinear)
weight <- cbind(weight, malefemale)
gfit3 <- lm(bwt ~ male+ female + age - 1, weight, x = T) ## have to remove intercept since we uinclude both dummies 

summary(gfit3)

# Results
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# male   -1610.28     786.08  -2.049   0.0532 .  
# female -1773.32     794.59  -2.232   0.0367 *  
# age      120.89      20.46   5.908 7.28e-06 ***

## How to interpret?
sum(weight$isfemale)
sum(weight$male)
## for a baby with mean age
meanageonly <- mean(weight$age)*120
mean_age_male = meanageonly + gfit3$coefficients[1]
mean_age_female = meanageonly + gfit3$coefficients[2]
## Hence: we can find weight from age first, then apply the offset due to male or female 
## in this case, no clear idea of intercept 

## Let us see the data and the interaction btn age and gender
ggplot(weight, aes(x = age, y = bwt, col = as.factor(male))) + geom_point() +
  xlim(0, max(weight$age) + 10)

## Check that gfit 2 and 3 are the same 
fitted(gfit2) ## fitted values
all.equal(gfit2$fitted.values, fitted(gfit2)) ## fitted() function and $fitted.values attribute returns same output

all.equal(fitted(gfit2), fitted(gfit3)) ## the two models, despite the different initial parameterisation, are the same!

## Make predictions 
new.data2 <- data.frame(isfemale = 1, age = 38)
p1 <- predict(gfit2, new.data2)
new.data22 <- data.frame(female = 1, age = 38, male = 0)
p2 <- predict(gfit3, new.data22) 
all.equal(p1, p2) ## the predictions are the same! 
# because they are fundamentally the same data 

## what is the difference then when we fit all levels of the categorical factor?
summary(gfit2) 
# Adjusted R2 = .6057
# F stat = 18.62 for 2,21 DF 
summary(gfit3) 
# Adjusted R2 = .9965 -- much higher when we fit all dummies!
# F stat = 2258 for 2,21 DF -- insanely higher 
# this is because we fitted in gender male and female as numeric instead of factor
# we have "more variables" in that model 

## -- Fit as.factor (gfit4)
gfit4 <- lm(bwt ~ age+ as.factor(isfemale), weight, x = T)
summary(gfit4)
anova(gfit4)
gfit4$x
fitted(gfit4)

## compare
gfit2$x ## the 'overfitted' model
gfit3$x ## as.factor(gender) model 

## -- Interaction (gfit5)
# as we've seen from the plot, there might be interaction
# i.e. difference of the age effect varying the other factor: gender
gfit5 <- lm(bwt ~ age * as.factor(isfemale),x=TRUE,data=weight)
summary(gfit5) ## interaction is statistically weak 

# Results
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              -1268.67    1114.64  -1.138 0.268492    
# age                        111.98      29.05   3.855 0.000986 ***
# as.factor(isfemale)1      -872.99    1611.33  -0.542 0.593952    
# age:as.factor(isfemale)1    18.42      41.76   0.441 0.663893    

anova(gfit5) ## Interaction ss accounts for a small proportion of total ss 

## Comparing two models using ANOVA
## This only works if models are nested -- here, gfit4 is nested version of gfit5 (where interaction = 0)
## i.e. gfit4 is more restricted than gfit5
comparison4n5 <- anova(gfit4, gfit5) ## btn no interaction and interaction
(comparison4n5)
# Returns an F stat of .1945 (very small) -- large p value of 66%
# Hence cannot reject gfit4 in favour of gfit5

