# Poli 281 Assignment 3
# Max Goetz

rm(list = ls())

library(dplyr)
library(ggplot2)

setwd("C:/Users/13366/Downloads/poli281") # Update as appropriate, of course.

anes <- read.csv("anes.csv", stringsAsFactors = TRUE)

# Code for Q1:
# created a new variable without the missing values in vote_gop and race columns
demraceyear <- anes %>% 
  filter(!is.na(vote_gop)) %>%
  filter(race!="")
# created a new variable that grouped the data by year and race then found the proportion of people who voted democrat
demvote_share <- demraceyear %>%
  group_by(year, race) %>%
  summarize(dem = 1 - mean(vote_gop, na.rm = TRUE))
# graphed this by race
demvote_share_p <- ggplot(demvote_share, aes(x=year, y=dem)) + geom_line() + facet_wrap(~race) + theme_bw()

# Code for Q2:
# made a new age column where ages were sorted into groups
anes$age_cat <- ""
anes$age_cat[anes$age >= 17 & anes$age <= 29] <- "17-29"
anes$age_cat[anes$age >= 30 & anes$age <= 39] <- "30-39"
anes$age_cat[anes$age >= 40 & anes$age <= 49] <- "40-49"
anes$age_cat[anes$age >= 50 & anes$age <= 59] <- "50-59"
anes$age_cat[anes$age >= 60] <- "60+"
# filtered for people who didn't put an age or didn't vote
demrace <- anes %>% 
  filter(!is.na(vote_gop)) %>%
  filter(age!="")
#grouped by year, age category. Found the proportion of those who voted democrat
dvote_share <- demrace %>%
  group_by(year, age_cat) %>%
  summarize(dem = 1 - mean(vote_gop, na.rm = TRUE))
# graphed it by year
dv_share_p <- ggplot(dvote_share, aes(x=age_cat, y=dem)) + geom_bar(stat = 'identity') + facet_wrap(~year) + theme_bw()

# Code for Q3:
# filtered anes for the year 2020
dvs_2020 <- anes %>%
  filter(year == 2020)
# graphed the age distribution from the 2020 election
dvs_2020_p <- ggplot(dvs_2020, aes(x = age)) + geom_histogram() + theme_bw()


# Code for Q4:
# filtered anes for year 2020 and removed rows with no gltherm data
gl2020 <- anes %>%
  filter(year == 2020) %>%
  filter(!is.na(gltherm))
# plotted this data
gl2020_p <- ggplot(gl2020, aes(x = age, y = gltherm)) +
  geom_jitter(size=.1) + stat_smooth()

# Code for Q5:
# created a variable that tells if someone is a democrat or republican using a loop
dr <- c()
for (row in 1:nrow(anes)) {
  if (anes[row, "democrat"] == 1) {
    dr[row] = "democrat"
  } else if (anes[row, "republican"] == 1) {
    dr[row] = "republican"
  }
}

# created a variable that stores 1 for those who voted for the other party's presidential candidate and 0 for those who voted for their parties presidential candidate
outvote <- c()

for (row in 1:length(dr)) {
  if (is.na(anes[row, "vote_gop"]) || is.na(dr[row])) {
    ""
  } else {  
    if (dr[row] == "democrat" && anes[row, "vote_gop"] == 0) {
    outvote[row] = 0
  } else if (dr[row] == "democrat" && anes[row, "vote_gop"] == 1) {
    outvote[row] = 1
  } else if (dr[row] == "republican" && anes[row, "vote_gop"] == 1) {
    outvote[row] = 0
  } else if (dr[row] == "republican" && anes[row, "vote_gop"] == 0) {
    outvote[row] = 1
    }
  }
}
# binded them to anes
anes <- cbind(anes, outvote)
anes <- cbind(anes, dr)

# created a new variable that grouped anes by year and by party then filtered outvote based on if people voted for or against their party's candidate. Summarized the data and graphed it.
cross_vote <- anes %>%
  group_by(year, dr) %>%
  filter(outvote == 0 | outvote == 1) %>%
  summarize(prop = mean(as.numeric(outvote)))

cross_vote_p <- ggplot(cross_vote, aes(x = year, y = prop, color = dr)) +
  geom_line()

# Code for Q6:
# filtered the data to the year 2020 and removed independents
init2020 <- anes %>%
  filter(year == 2020) %>%
  filter(!(independent == 1))

# grouped the data by state then found the proportion of republicans in each state
prop2020 <- init2020 %>%
  group_by(state) %>%
  summarize(rprop = sum(republican) / (sum(republican) + sum(democrat)))
# graphed the data
prop_2020_p <- ggplot(prop2020, aes(x=state, y=rprop)) + geom_bar(stat = "identity") + theme_bw()

# Code for Q7:
# arranged the data within prop2020 then changed it to be ordered from highest proportion of republicans to lowest
prop2020 <- prop2020 %>%
  arrange(rprop)

prop2020$state <- factor(prop2020$state, levels = prop2020$state)

colors = c()
# made a new color variable to give each data value a color based on proportion
for (row in 1:nrow(prop2020)) {
  if (prop2020[row, "rprop"] <= .45) {
    colors[row] = "blue"
  } else if (prop2020[row, "rprop"] >= .55) {
    colors[row] = "red"
  } else {
   colors[row] = "green"
  }
}

#graphed this

prop_2020_ordered_p <- ggplot(prop2020, aes(x=rprop, y=state, fill=factor(rprop))) + geom_bar(stat = "identity") + geom_vline(xintercept=0.5) + scale_fill_manual(values = colors) 