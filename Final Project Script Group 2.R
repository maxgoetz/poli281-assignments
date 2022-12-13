rm(list = ls())
setwd("C:/Users/13366/Downloads/poli281")
ces <- read.csv("ces20.csv")

library(dplyr)
library(ggplot2)


### Section II
prop.table(table(ces$wait))
mean(ces$wait)
median(ces$wait)

ces$more10 <- 0
ces$more10[ces$wait >= 3 & ces$wait != 6] <- 1
ces$more10[ces$wait < 3] <- 0
ces$more10[ces$wait ==6] <- NA


##Section III
states <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")

states_wait <- ces %>%
  group_by(state) %>%
  summarize(prop_wait = mean(more10, na.rm = TRUE)) 

states_wait_sorted <- states_wait %>%
  arrange(desc(prop_wait))

states_wait_sorted$state <- factor(states_wait_sorted$state, levels = states_wait_sorted$state) 

states_wait_graph <- ggplot(states_wait_sorted, aes(x=state, y=prop_wait)) + geom_col() + theme(axis.text.x = element_text(angle = 90)) + labs(y= "Proportion Waiting More Than 10 Minutes", x = "State") + labs(title = "Wait Time Across States")


##Section IV
states_wait_region <- ces %>%
  group_by(state, region) %>%
  summarize(prop_wait = mean(more10, na.rm = TRUE))

states_wait_region_sorted <- states_wait_region %>%
  arrange(desc(prop_wait))

states_wait_region_sorted$state <- factor(states_wait_region_sorted$state, levels = states_wait_region_sorted$state)

region <- states_wait_region_sorted

for (i in 1:nrow(states_wait_region_sorted)) {
  if (states_wait_region_sorted[i, "region"] == "S") {
    region[i, "color"] = "pink"
  } else if (states_wait_region_sorted[i, "region"] == "NE") {
    region[i, "color"] = "green"
  } else if (states_wait_region_sorted[i, "region"] == "MW") {
    region[i, "color"] = "blue"
  } else if (states_wait_region_sorted[i, "region"] == "W") {
    region[i, "color"] = "yellow"
  }
}

states_wait_graph_region <- ggplot(states_wait_region_sorted, aes(x=state, y=prop_wait)) + geom_col(fill = region$color) + theme(axis.text.x = element_text(angle = 90)) + labs(y= "Proportion Waiting More Than 10 Minutes", x = "State") + labs(title = "Wait Time Across States Including Regions")



### SECTION V
ces$conserv_vote <- 0
ces$conserv_vote[ces$vote2020 == 1] <- 1
ces$conserv_vote[ces$vote2020 == 2] <- 0
ces$conserv_vote[ces$vote2020 > 2] <- NA

VoteRepublican <- ces %>%
  filter(!is.na(conserv_vote)) %>%
  filter(!is.na(more10))

prop.table(table(VoteRepublican$more10, VoteRepublican$conserv_vote), margin = 2)
prop.table(table(ces$more10, ces$conserv_vote), margin = 2)



### Section VI
ces$race_5<-""
ces$race_5[ces$race==1] <- "White Non-Hispanic"
ces$race_5[ces$race==2] <- "Black"
ces$race_5[ces$race==3] <- "Hispanic"
ces$race_5[ces$race==4] <- "Asian"
ces$race_5[ces$race>4] <- "Other"

More10Race <- ces %>%
  filter(!is.na(more10)) %>%
  group_by(race_5) %>%
  summarize(propmore10 = mean(more10, na.rm=TRUE))

More10Race <- More10Race %>%
  arrange(propmore10)  

More10Race$race_5 <- factor(More10Race$race_5, levels = More10Race$race_5)

RaceWait_Plot<- ggplot(More10Race, aes(x=race_5, y=propmore10)) + geom_col() + theme_bw() +labs(y= "Proportion Waiting More Than 10 Minutes", x = "Race") + labs(title = "Wait Time by Race")


### Section VII
ces$faminc_4 <- ""
ces$faminc_4[ces$faminc <= 3] <- "Less than $30,000" #Under poverty line
ces$faminc_4[ces$faminc >3 & ces$faminc<7 ] <- "$30,000-$59,000"  #Lower middle class
ces$faminc_4[ces$faminc >=7 & ces$faminc<11] <- "$60,000-$119,000" #Middle class
ces$faminc_4[ces$faminc >=11] <- "$120,000 and greater" #Upper middle class+
ces$faminc_4[ces$faminc == 97] <-NA
ces$faminc_4[is.na(ces$faminc)] <-NA

prop.table(table(ces$faminc_4))

More10Income <- ces %>%
  filter(!is.na(more10)) %>%
  filter(!is.na(faminc_4)) %>%
  group_by(faminc_4) %>%
  summarize(more10prop = mean(more10)) 

More10Income <- More10Income %>%
  arrange(more10prop) 

More10Income$faminc_4 <- factor(More10Income$faminc_4, levels = More10Income$faminc_4)

FamInc_Plot<- ggplot(More10Income, aes(x=faminc_4, y=more10prop)) + geom_col() + theme_bw() +labs(y= "Proportion Waiting More Than 10 Minutes", x = "Family Income") +labs(title = "Wait Time by Family Income")

#Scatterplot
More10Incomeline <- ces %>%
  filter(!is.na(more10)) %>%
  filter(!is.na(faminc)) %>%
  filter(faminc!=97) %>%
  group_by(faminc) %>%
  summarize(more10prop = mean(more10))

ggplot(More10Incomeline, aes(x=faminc, y=more10prop)) + geom_point() + theme_bw() + labs(y= "Proportion Waiting More Than 10 Minutes", x = "Family Income") + labs(title = "Wait Time Across All Income Categories")

##Section VIII
More10RaceAndIncome <- ces %>%
  filter(!is.na(faminc_4)) %>%
  group_by(race_5, faminc_4) %>%
  summarize(propmore10 = mean(more10, na.rm=TRUE))


More10RaceAndIncome$faminc_4 <- factor(More10RaceAndIncome$faminc_4, levels = c("Less than $30,000", "$30,000-$59,000", "$60,000-$119,000", "$120,000 and greater"))

SE_graph <- ggplot(More10RaceAndIncome, aes(x = race_5, y = propmore10)) + geom_col() + theme_bw() + facet_wrap(~faminc_4) + labs (y="Proportion Waiting More Than 10 Minutes", x = "Race") + labs(title = "Wait Time by Race and Income")


##Section IV
incomeCountyFiltered <- ces %>%
  filter(income_county < 150)

income_graph <- ggplot(incomeCountyFiltered, aes(x=income_county)) + geom_histogram() + theme_bw() + labs (y="Respondents", x = "County Income") + labs(title = "County Income of Respondents")

density <- c()
for (row in 1:nrow(ces)) {
  density[row] = ces[row, "county_pop"] / ces[row, "land_area"]
}

ces <- cbind(ces, density)

densityFiltered <- ces %>%
  filter(density < 10000)

density_graph <- ggplot(densityFiltered, aes(x = density)) + geom_histogram() + theme_bw() + labs (y="Counties", x = "Population Density") + labs(title = "Population Density of Counties")

ces$black <- c()
ces$hispanic <- c()
ces$asian <- c()
ces$other <- c()
for (row in 1:nrow(ces)) {
  if (ces[row, "race"] == 2) {
    ces[row, "black"] = 1
  } else {
    ces[row, "black"] = 0
  }
} 

for (row in 1:nrow(ces)) {
  if (ces[row, "race"] == 3) {
    ces[row, "hispanic"] = 1
  } else {
    ces[row, "hispanic"] = 0
  }
} 

for (row in 1:nrow(ces)) {
  if (ces[row, "race"] == 4) {
    ces[row, "asian"] = 1
  } else {
    ces[row, "asian"] = 0
  }
} 

for (row in 1:nrow(ces)) {
  if (ces[row, "race"] == 5 | ces[row, "race"] == 6 | ces[row, "race"] == 7 | ces[row, "race"] == 8) {
    ces[row, "other"] = 1
  } else {
    ces[row, "other"] = 0
  }
} 

ces$wait_reg <- c()
for (row in 1:nrow(ces)) {
  if (ces[row, "wait"] != 6) {
    ces[row, "wait_reg"] = ces[row, "wait"]
  } else {
    ces[row, "wait_reg"] = NA
  }
}

famincFiltered <- ces %>%
  filter(!is.na(ces$faminc))

ces$faminc_reg <- c()
for (row in 1:nrow(ces)) {
  if (!is.na(ces[row, "faminc"])) {
    if (ces[row, "faminc"] != 97) {
      ces[row, "faminc_reg"] = ces[row, "faminc"]
    } else {
      ces[row, "faminc_reg"] = NA
    }
  } else {
    ces[row, "faminc_reg"] = NA
  }
}

model1 <- lm(wait_reg ~ black + hispanic + asian + other, data = ces)
model2 <- lm(wait_reg ~ black + hispanic + asian + other + faminc_reg + income_county + density, data = ces)
summary(model1)
summary(model2)
nobs(model1)
nobs(model2)