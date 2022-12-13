rm(list = ls())
setwd("C:/Users/13366/Downloads/poli281")
stops <- read.csv("stops.csv")

# Q1
table(stops$searchoccur)
5681 / (5681 + 11055)
mean(stops$searchoccur)
fit_intercept_only <- lm(searchoccur ~ 1, data=stops) # This is how you estimate a regression with NO independent variables--just the intercept term. It essentially asks and answers the question, "What number, if multiplied by 1, would be our best guess for the value of Y in this dataset? data=stops at the end is just a way to avoid having to specify the data frame for each variable I reference.
summary(fit_intercept_only)
nobs(fit_intercept_only)

# Q2
fit1 <- lm(searchoccur ~ age, data = stops)
summary(fit1)
nobs(fit1)

# Q3
fit1$coefficients[1] + fit1$coefficients[2]*20
fit1$coefficients[1] + fit1$coefficients[2]*30
fit1$coefficients[1] + fit1$coefficients[2]*40
fit1$coefficients[1] + fit1$coefficients[2]*50
fit1$coefficients[1] + fit1$coefficients[2]*60

fit1$coefficients[1] + fit1$coefficients[2]*-25
fit1$coefficients[1] + fit1$coefficients[2]*700



# Q4 - regression
stops$age_r <- (stops$age - 18) / 42 
summary(stops$age_r) # Check min and max
fit2 <- lm(searchoccur ~ age_r, data = stops)
summary(fit2)
nobs(fit2)

# Q4 - Predicted values. We could do what we did above IF we transformed the raw ages into the new scaling of age. E.g., 25 becomes (25 - 18) / 42 = 0.166. I do that below, but I integrate it into a loop, so you can see how that would work.
ages <- c(20, 30, 40, 50, 60) # Ages we want
pred_val <- rep(NA, times=length(ages)) # Place to store the predicted values we will calculate
results <- data.frame(ages, pred_val) # bind the two things above together

for (age in results$ages) { # Loop over the listed ages
  results[ages==age,2] <- fit2$coefficients[1] + fit2$coefficients[2]*((age - 18) / 42) # Calculate the predicted value for each. We need to apply the same transformation--minus 18, divide by 42--to find the appropriate x value corresponding to the raw ages.
}
results # Look at results.


#Q5
stops$male <- 0
stops$male[stops$sex=="Male"] <- 1

cor(stops$searchoccur, stops$male) # positive
cor(stops$age_r, stops$male) # Negative. Fairly small but maybe large enough to matter.

male <- subset(stops, sex=="Male")
female <- subset(stops, sex=="Female")

fit3 <- lm(male$searchoccur ~ male$age_r)
fit4 <- lm(female$searchoccur ~ female$age_r)
summary(fit3)
nobs(fit3)
summary(fit4)
nobs(fit4)

# Q6
# The next two steps are because sex is a character vector, which does not work in regression.
stops$male <- 0 # create var
stops$male[stops$sex=="Male"] <- 1  # change to 1 if male
fit5 <- lm(searchoccur ~ age_r + male, data=stops)
summary(fit5)
nobs(fit5)

# Q7
mean(stops$searchoccur[stops$race=="White"])
mean(stops$searchoccur[stops$race=="Black"])
mean(stops$searchoccur[stops$race=="Other"])
# (Could also have used dplyr and group_by)

stops$black <- 0
stops$black[stops$race=="Black"] <- 1
stops$other <- 0
stops$other[stops$race=="Other"] <- 1

fit6 <- lm(stops$searchoccur ~ stops$black + stops$other)
summary(fit6)
nobs(fit6)