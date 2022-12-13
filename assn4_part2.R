rm(list=ls())
setwd("C:/Users/13366/Downloads/poli281") # You need to update this
rumor <- read.csv("rumor.csv", stringsAsFactors = TRUE)

# Code for Q1
#creating a new table based on how hofmann was described and by education
table(rumor$educ, rumor$hof_outgroup) 
prop.table(table(rumor$educ, rumor$hof_outgroup)) 
prop.table(table(rumor$educ, rumor$hof_outgroup), margin=2) 

#making a table based on race and how hofmann was described
table(rumor$race_ethnic, rumor$hof_outgroup) 
prop.table(table(rumor$race_ethnic, rumor$hof_outgroup)) 
prop.table(table(rumor$race_ethnic, rumor$hof_outgroup), margin=2) 
# Code for Q2. Giving you a head start.
democrats <- rumor[rumor$democrat==1,]
republicans <- rumor[rumor$democrat==0,]

di <- c()
id <- 0
do <- c()
od <- 0
#getting rid of all na values in the dataset. Using a loop to sort through all the democrats then assign them to a group based on if they were told hofmann was a dem or not
for (value in 1:nrow(rumor)) {
  if (is.na(rumor[value, "democrat"])) {
  } else if (rumor[value, "democrat"] == 1 & rumor[value, "hof_outgroup"] == 0) {
    id <- id + 1
    di[id] = rumor[value, "happened"]
  } else if (rumor[value, "democrat"] == 1 & rumor[value, "hof_outgroup"] == 1) {
    od <- od + 1
    do[od] = rumor[value, "happened"]
  }
}

mean(di)
mean(do)
#finding the mean of these vectors

ri <- c()
ir <- 0
ro <- c()
or <- 0
#doing the same thing for republicans
for (value in 1:nrow(rumor)) {
  if (is.na(rumor[value, "republican"])) {
  } else if (rumor[value, "republican"] == 1 & rumor[value, "hof_outgroup"] == 0) {
    ir <- ir + 1
    ri[ir] = rumor[value, "happened"]
  } else if (rumor[value, "republican"] == 1 & rumor[value, "hof_outgroup"] == 1) {
    or <- or + 1
    ro[or] = rumor[value, "happened"]
  }
}

mean(ri)
mean(ro)