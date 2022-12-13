# Student Name: Max Goetz
# Poli281, Homework Assignment 2

rm(list=ls()) # Let's start with an empty workspace.

### Import the Dataset Programatically
setwd("C:/Users/13366/Downloads/poli281")

state_taxes <- read.csv("state_taxes.csv", stringsAsFactors = TRUE)

View(state_taxes)

### Code for Question 1:
# Created a new vector and used a for loop to iterate through state_taxes. Created a sum variable to find the total number of sin taxes in each state and assigned that value to the current index of substance tax. Binded substance_tax to state tax and put it in order from highest to lowest.
substance_tax <- c()
for (i in 1:nrow(state_taxes)) {
  sum <- as.numeric(state_taxes[i, "Alcoholic_Beverage_Tax"]) + as.numeric(state_taxes[i, "Tobacco_Tax"])
  substance_tax[i] <- sum
}
state_taxes <- cbind(state_taxes, substance_tax)

sub_order <- order(state_taxes$substance_tax, decreasing = T)
state_taxes <- state_taxes[sub_order,]
head(state_taxes)

### Code for Question 2:
# Created a new data frame called low_tax from state tax and substance tax. Found the number of rows of this subset.
low_tax <- data.frame(subset(state_taxes, substance_tax < 100000))
nrow(low_tax)

### Code for Question 3:
# Created a new perc_sub vector. Used a for loop to assign the percent of substance taxes each state uses to perc_sub and binded it to state_taxes. Then put it in order and printed it.
perc_sub <- c()
for (i in 1:nrow(state_taxes)) {
  perc_sub[i] <- 100 * (as.numeric(state_taxes[i, "substance_tax"]) / as.numeric(state_taxes[i, "Total_Revenue"]))
}
state_taxes <- cbind(state_taxes, perc_sub)

ps_order <- order(state_taxes$perc_sub, decreasing = T)

print(state_taxes[head(ps_order),])

### Code for Question 4:
# Created a new subset for each region then found the mean of substance_tax and perc_sub for each.
NC <- subset(state_taxes, Region == "NorthCentral")
mean(NC$substance_tax)
mean(NC$perc_sub)

NE <- subset(state_taxes, Region == "Northeast")
mean(NE$substance_tax)
mean(NE$perc_sub)

W <- subset(state_taxes, Region == "West")
mean(W$substance_tax)
mean(W$perc_sub)

S <- subset(state_taxes, Region == "South")
mean(S$substance_tax)
mean(S$perc_sub)
### Code for Question 5:
# part a
# Created a new total_rev and total_tax vector. Divided total_tax by total_rev then multiplied by 100 to find the tax reliance. Binded it to state_taxes.
total_rev <- state_taxes[,"Total_Revenue"]
total_tax <- state_taxes[, "Total_Taxes"]

tax_rel <- 100 * (total_tax / total_rev)
state_taxes <- cbind(state_taxes, tax_rel)

#part b
# Created a new vector for each region. Used a for loop with conditionals to assign the tax reliance for each region and assigned it to the vectors. Found the mean of each vector.
NC_trel <- c()
NE_trel <- c()
S_trel <- c()
W_trel <- c()
for (i in 1:nrow(state_taxes)) {
  if (state_taxes$Region[i] == "NorthCentral"){
    NC_trel[i] <- tax_rel[i]
  } else if (state_taxes$Region[i] == "Northeast"){
    NE_trel[i] <- tax_rel[i]
  } else if (state_taxes$Region[i] == "South"){
    S_trel[i] <- tax_rel[i]
  } else if (state_taxes$Region[i] == "West"){
    W_trel[i] <- tax_rel[i]
  }
}
mean(W_trel, na.rm = TRUE)
mean(S_trel, na.rm = TRUE)
mean(NE_trel, na.rm = TRUE)
mean(NC_trel, na.rm = TRUE)

### Code for Question 6:
# part a
# Used a for loop to find the total number of values from year 2012 with tax_rel > 40. If the conditionals were satisfied, 1 was added to the states variable.
states <- 0
for (i in 1:nrow(state_taxes)) {
  if (state_taxes$Year[i] == 2012 & state_taxes$tax_rel[i] > 40) {
    states <- states + 1
  }
}

# part b
# Did the same thing as part a but changed the conditions to year 2013 and medium tax_rel.
states <- 0
for (i in 1:nrow(state_taxes)) {
  if (state_taxes$Year[i] == 2013 & state_taxes$tax_rel[i] > 33 & state_taxes$tax_rel[i] < 40) {
    states <- states + 1
  }
}

# part c
# Did the same thing as part a but changed the conditions to year 2012, low tax_rel, and the south region.
states <- 0
for (i in 1:nrow(state_taxes)) {
  if (state_taxes$Year[i] == 2012 & state_taxes$tax_rel[i] < 33 & state_taxes$Region[i] == "South") {
    states <- states + 1
  }
}

# part d
# Did the same thing as part a but changed the conditions to year 2012, the south region, and high tax_rel.
states <- 0
for (i in 1:nrow(state_taxes)) {
  if (state_taxes$Year[i] == 2012 & state_taxes$Region[i] == "South" & (state_taxes$tax_rel[i] > 40 | state_taxes$tax_rel[i] < 33)) {
    states <- states + 1
  }
}