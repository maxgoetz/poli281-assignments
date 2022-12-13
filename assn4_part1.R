rm(list=ls())
setwd("C:/Users/13366/Downloads/poli281")
grades <- read.csv("grades.csv", stringsAsFactors = TRUE)

library(dplyr)
library(ggplot2)

# Q1: Proportion doing paper notes
sum(grades$paper == "Paper")
mean(grades$paper == "Paper")
# Q2: Calculate TE for each individual
grades$TE <- grades$Y1 - grades$Y0
#assigning the difference between Y1 and Y0 to column TE
ggplot(grades, aes(x=TE)) + geom_histogram() + theme_bw()
#creating a plot of TE

# Q3: Calculate SATE
mean(grades$Y1) - mean(grades$Y0)

# Q4: Difference in means
p <- 0
c <- 0
ps <- c()
cs <- c()
for (value in 1:2000) {
  if (grades[value, "paper"] == "Paper") {
    p <- p + 1
    ps[p] = grades[value, "Y1"]
  } else if (grades[value, "paper"] == "Computer") {
    c <- c + 1
    cs[c] = grades[value, "Y0"]
  }
}
#using a loop to assign the scores for paper from people who prefer to take notes on paper and for computer for from people who prefer to take notes on computer to 2 vectors

sum(ps)/p - sum(cs)/c


# Q5: (No code)

# Illustrative aside on proportions
# This is to remind you how the proportions command (discussed in Imai) works. The first line shows counts by cell. The second line divides those numbers by the total number of observations. The third line figures out the proportions BY COLUMN, so that the column proportions total to 100.
table(grades$athlete, grades$paper) 
prop.table(table(grades$athlete, grades$paper)) 
prop.table(table(grades$athlete, grades$paper), margin=2) 

# Q6: Proportions to fill in table: athletes, on-campus, and freshmen.
table(grades$campus, grades$paper) 
prop.table(table(grades$campus, grades$paper)) 
prop.table(table(grades$campus, grades$paper), margin=2) 

table(grades$year, grades$paper) 
prop.table(table(grades$year, grades$paper)) 
prop.table(table(grades$year, grades$paper), margin=2) 

# Q7: Random assignment
grades$rand_paper <- 0 # New column full of zeroes
rand_list <- sample(2000, size = 690) # Choose 690 students completely at random. 690 came from are actual proportion of paper users (34.5).
grades$rand_paper[rand_list] <- 1 # For the randomly chosen students, change the 0 to a 1

# Proportions, for randomly-constructed T and C groups
prop.table(table(grades$athlete, grades$rand_paper), margin=2) # I've done the first one for you.
prop.table(table(grades$campus, grades$rand_paper), margin=2)
prop.table(table(grades$year, grades$rand_paper), margin=2)

# Q8: (No code expected.)

# Q9: Difference-in-means under random assignment. Your mileage will vary but not by a lot.

p <- 0
c <- 0
ps <- c()
cs <- c()
for (value in 1:2000) {
  if (grades[value, "rand_paper"] == "1") {
    p <- p + 1
    ps[p] = grades[value, "Y1"]
  } else if (grades[value, "rand_paper"] == "0") {
    c <- c + 1
    cs[c] = grades[value, "Y0"]
  }
}
#creating 2 new vectors where people have been randomly assigned groups then putting their test scores based on their group in the vector

sum(ps)/p - sum(cs)/c


# Q10: Optional. See instructions.
estimand <- mean(grades$TE)

c_mean <- mean(grades$Y0[grades$paper=="Computer"]) 
t_mean <- mean(grades$Y1[grades$paper=="Paper"])

estimate_choice <- t_mean - c_mean 

grades$t_rand <- sample(c(0,1), size = nrow(grades), replace = TRUE)

c_mean <- mean(grades$Y0[grades$t_rand==0])
t_mean <- mean(grades$Y1[grades$t_rand==1])

estimate_random <- t_mean - c_mean 

results <- data.frame(estimate = NA)

for (i in 1:1000) {
  grades$t_rand <- sample(c(0,1), size = nrow(grades), replace = TRUE) 
  c_mean <- mean(grades$Y0[grades$t_rand==0])
  t_mean <- mean(grades$Y1[grades$t_rand==1])
  results[i,"estimate"] <- t_mean - c_mean
}

ggplot(results, aes(x=estimate)) + 
  geom_histogram(bins = 60) + 
  geom_vline(xintercept = estimand, color="green") + 
  geom_vline(xintercept = estimate_choice, color="blue") +
  theme_bw()