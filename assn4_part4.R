# Poli281 Assignment 3, Part 3
# Name: Max Goetz

# Code to import data for Part 3 (Need to update this):
rm(list=ls())
setwd("C:/Users/13366/Downloads/poli281")
salt <- read.csv("salt.csv")

# Code for Q1
salt[1, "accidents_17"] - salt[1, "accidents_18"]
# Code for Q2
#creating a new subset for the untreated cities
utreat <- subset(salt, city != "A")
(salt[1, "accidents_18"]) - (mean(utreat$accidents_18))
# Code for Q3 (Code allowed, but not required.)

# Code for Q4
