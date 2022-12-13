### Poli281, Spring 2022, Homework Assignment 1
# Student Name: Max Goetz
# Section (3:30 or 5pm): 3:30

rm(list=ls()) # Let's start with an empty workspace.

### No required packages

### Load data
# Note: Make sure you import the dataset *programmatically.* That is part of the assignment.

setwd("C:/Users/13366/Downloads/poli281") # Set working directory to the place where I saved the dataset. This will be different on your computer, since your folder structure is different than mine.

cong_votes <- read.csv("house_votes2020.csv", stringsAsFactors = TRUE) # Read the CSV file into an object called cong_vote. Remember what we learned in class about stringsAsFactors needing to be specified nowadays.

View(cong_votes) # Do the data look ok? In particular, did R recognize that the first row of the dataset represents column titles? Yes.


### Code for Question 1:
# Note: The easiest way to do this is with a command we haven't explicitly learned yet. But I bet you can figure out how to use it here. The command you want is "nrow()" All you need to do is figure out what belongs in the parentheses.
nrow(cong_votes)
# used nrow to find the total number of rows in the data set.

### Code for Question 2:
# Note: To answer this question, place the California-related rows alone in an object called "california". Then, count the number of rows in that object. You might want to review the Datacamp lesson focused on the subset() command.
california <- subset(cong_votes, subset = State == "California")
nrow(california)
# created a new data set called "california" by using the subset command to find the rows that had California as their State value

### Code for Question 3:
cong_votes[288, 3]
# found the cong_votes value at 288, 3

### Code for Question 4:
sum(cong_votes[, "Other"])
# used the sum keyword to find the total number of votes in the other column

### Code for Question 5:
sum(cong_votes[, 6:8])
# used the sum keyword to find the total number of votes in columns 6 through 8

### Code for Question 6:
(sum(cong_votes[, "Dem"])) / (sum(cong_votes[, 6:8]))
# found the sum of democratic votes and divided it by the sum of all the votes

### Code for Question 7:

t_dems <- subset(cong_votes, subset = Party == "D")
sum(t_dems[, "Fresh"] == 1) / nrow(t_dems)
# created a new data set of only democrats. Sorted this set for freshmen then divided the number of freshmen by the number of democrats.


### Code for Question 8:
# Note: For this question, remember that, when R evaluates conditional statements, it treats TRUE values as the number 1, and FALSE statements as the number 0. See the first three lines below. You want to extend that approach to apply to cong_votes
example <- factor(c("Dog", "Cat", "Dog", "Dog"))
example == "Dog"
mean(example == "Dog") # Evaluates to .75.
# Explanation of the above:
sum(c(TRUE, FALSE, TRUE, TRUE)) #this is what R is doing internally
mean(example == "Dog") # Evaluates to .75, since R has substituted 1, 0, 1, 1.

mean(cong_votes$Party == "D")
# created a boolean vector for when the party column == D. found the mean of this vector, resulting in the percentage of seats won by democrats

### NOTES FOR QUESTIONS 9 & 10:
# Mario Diaz-Balart in Florida 25 ran unopposed and so had no votes cast for or against him. To make your life easier for the remaining questions, we are deleting him from the dataset. To do this, run the code below:
cong_votes <- cong_votes[cong_votes$Winner!="Diaz-Balart, Mario",]

### Code for Question 9
# Part a
pDem <- c()
# initialized pDem to an empty vector

i <- 1
while (i <= 434) {
  pDem[i] <- (cong_votes[i, "Dem"] / sum(cong_votes[i, 6:7]))
  i <- i + 1
}
# used a loop to assign the pDem value of every row to pDem

cong_votes <- cbind(cong_votes, pDem)
# binded it to cong_votes

# Part b
mean(pDem)
# used mean to find the mean of pDem

435 * mean(pDem) - 435 * mean(cong_votes$Party == "D")
# used the mean value to find how many seats they would have won if they got the pDem percentage of votes

### Code for Question 10
nDem <- pDem - .05
# created a new vector that is all the pDem values minus .05

nWin <- c()
i <- 1
while (i < 435) {
  if (nDem[i] < .5) {
    nWin[i] <- "R"
  } else {
    nWin[i] <- "D"
  }
  i <- i + 1
}
# used a while loop to find out which party would have won each seat if the votes for democrats went down 5 points

R <- 0
D <- 0
for (value in nWin) {
  if (value == "R") {
    R <- R + 1
  } else {
    D <- D + 1
  }
}
D / (R + D)
# used a loop to find the total number of democrats and republicans in the data set. from there, found the proportion of democrats that would have still won

### Code for Question 11
# Part a
i <- 284
j <- 1
NC_CD <- c()
NC_Winner <- c()
NC_Fresh <- c()
NC_Party <- c()
NC_Dem <- c()
NC_Rep <- c()
NC_Other <- c()
NC_pDem <- c()
while (i < 297) {
  NC_CD[j] <- cong_votes[i, "CD"]
  NC_Winner[j] <- cong_votes[i, "Winner"]
  NC_Fresh[j] <- cong_votes[i, "Fresh"]
  NC_Party[j] <- cong_votes[i, "Party"]
  NC_Dem[j] <- cong_votes[i, "Dem"]
  NC_Rep[j] <- cong_votes[i, "Rep"]
  NC_Other[j] <- cong_votes[i, "Other"]
  NC_pDem[j] <- cong_votes[i, "pDem"]
  j <- j + 1
  print(i)
  i <- i + 1
}


NC_df <- data.frame(NC_CD, NC_Winner, NC_Fresh, NC_Party, NC_Dem, NC_Rep, NC_Other, NC_pDem)
# created a new data frame using only information about north carolina by using new variables and a while loop

sum(NC_df$NC_Party == 1) / (sum(NC_df$NC_Party == 2) + sum(NC_df$NC_Party == 1))
# found the percentage of seats democrats hold

# Part b
sum(NC_df$NC_Dem) / sum(NC_df[5:7])
# used sum function to find the percentage of congressional votes that were cast for democratic candidates

# Part c
NC_df

i <- 1
j <- 0
k <- 0
while (i < 13) {
  if (NC_df[i, "NC_pDem"] < .5) {
    if (NC_df[i, "NC_pDem"] > j){
      j <- NC_df[i, "NC_pDem"]
    } 
    else if (NC_df[i, "NC_pDem"] > k){
      k <- NC_df[i, "NC_pDem"]
    }
  }
  i <- i + 1
}
j
k
# used a while loop to find the two districts that republicans won with the highest percentage of votes for democrats

o <- 1
while (o <= 13) {
  if (NC_df$NC_pDem[o] == k) {
    print(o)
  }
  o <- o + 1
}
# used another while loop to find the index number of the district with the second highest pDem number in districts republicans won

h <- NC_Dem[9]
g <- 1
r <- 1
e <- NC_Rep[9]
while (h * g <= e * r) {
  g <- g + .001
  r <- r - .001
}
g - 1
# used a while loop to find the percentage of voters that would have to change their vote for democrats to win 7 districts

NC_Dem * g > NC_Rep * r