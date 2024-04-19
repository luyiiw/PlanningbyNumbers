#ASSIGNMENT 3####
# IN CLASS DEMO VER###
rm(list = ls())
library(haven)
library(arm)
library(tidyverse)

#Question01####
##Data loading
load("cpln505_assignment3_voting_data_abb.rda")

dat01 <- dat.voting %>% filter(VCF0004 == 2012 | VCF0004 == 2016) %>%
  rename(candidate = VCF0704a, income=VCF0114, year = VCF0004, religion=VCF0128, education=VCF0140a, party=VCF0302, race = VCF0105a, age=VCF0101, gender=VCF0104) %>%
  select(year, candidate, income, year, religion, education, party, race, age, gender) %>%
  filter(race != 9, candidate != 0, income != 0, religion != 0, !education %in% c(8, 9), !party %in% c(3,4,8, 9), !age <=17, gender !=3) %>%
  mutate(
    race_cat = as.factor(recode(race,
                                     `1` = "white",
                                     `2` = "black",
                                     `3` = "other",
                                     `4` = "other",
                                     `5` = "Hispanic",
                                     `6` = "other")),
    candidate_cat=as.factor(recode(candidate,
                                   `1` = "Democrat",
                                   `2` = "Republican")),
    income_cat=as.factor(recode(income,
                              `1`= "lower income",
                              `2`= "lower income",
                              `3`= "middle income",
                              `4`= "high income",
                              `5`= "high income")),
    religion_cat=as.factor(recode(religion,
                                  `1`="Protestant",
                                  `2`="Catholic",
                                  `3`="Jewish",
                                  `4`="Other")),
    education_cat=as.factor(recode(education,
                                   `1` = "Below HS",
                                   `2` = "Below HS",
                                   `3` = "HS",
                                   `4` = "HS",
                                   `5` = "Some College",
                                   `6` = "BS Degree",
                                   `7` = "Graduate Degree")),
    party_cat=as.factor(recode(party,
                               `1` = "Republican",
                               `2` = "Independent",
                               `5` = "Democrat")),
    gender_cat=as.factor(recode(gender,
                                `1`="Male",
                                `2`="Female")))%>%
  select(-candidate,-income,-religion,-education,-party,-race,-gender)
                    
##recategorizing
dat02 <- dat.voting %>% filter(VCF0004 == 2012 | VCF0004 == 2016) %>%
  rename(candidate = VCF0704a, income=VCF0114, year = VCF0004, religion=VCF0128, education=VCF0140a, party=VCF0302, race = VCF0105a, age=VCF0101, gender=VCF0104, south=VCF0113) %>%
  select(year, candidate, income, year, religion, education, party, race, age, gender, south) %>%
  filter(race != 9, candidate != 0, income != 0, religion != 0, !education %in% c(8, 9), !party %in% c(3,4,8, 9), !age <=17, !gender %in% c(0,3)) %>%
  mutate(
    race_cat = as.factor(recode(race,
                                `1` = "White",
                                `2` = "Black",
                                `3` = "other",
                                `4` = "other",
                                `5` = "Hispanic",
                                `6` = "other")),
    candidate_cat=as.factor(recode(candidate,
                                   `1` = "Democrat",
                                   `2` = "Republican")),
    income_cat=as.factor(recode(income,
                                `1`= "lower income",
                                `2`= "lower income",
                                `3`= "middle income",
                                `4`= "high income",
                                `5`= "high income")),
    religion_cat=as.factor(recode(religion,
                                  `1`="Protestant",
                                  `2`="Catholic",
                                  `3`="Jewish",
                                  `4`="Other")),
    education_cat=as.factor(recode(education,
                                   `1` = "Below HS",
                                   `2` = "Below HS",
                                   `3` = "HS",
                                   `4` = "HS",
                                   `5` = "Some College",
                                   `6` = "BS Degree",
                                   `7` = "Graduate Degree")),
    party_cat=as.factor(recode(party,
                               `1` = "Republican",
                               `2` = "Independent",
                               `5` = "Democrat")),
    gender_cat=as.factor(recode(gender,
                                `1`="Male",
                                `2`="Female")),
    south_cat=as.factor(recode(south,
                               `1`="South",
                               `2`="Non-South")),
   age_cat=case_when(
    age >= 18 & age <= 24 ~ "18-24",
     age >= 25 & age <= 34 ~ "25-34",
     age >= 35 & age <= 44 ~ "35-44",
     age >= 45 & age <= 59 ~ "45-59",
     age >= 60            ~ "60+" ))%>%
  select(-candidate,-income,-religion,-education,-party,-race,-gender,-age,-south)



library(dplyr)
library(tidyr)
library(ggplot2)

###Year by Race###
ggplot(dat02, aes(x = year, fill = race_cat)) +
  geom_bar(position = "dodge", stat="count") +
  labs(title = "Count of People in Each Race Group by Year",
       x = "Race Group",
       y = "Count",
       fill = "Race Group",
       caption = "Figure 1.1")+
  scale_x_continuous(breaks = c(2012, 2016), labels = c("2012", "2016")) + # Set x-axis breaks
  theme_minimal()


###Year by Gender###
ggplot(dat02, aes(x = year, fill = gender_cat)) +
  geom_bar(position = "dodge", stat="count") +
  labs(title = "Count of People in Each Gender Group by Year",
       x = "Gender Group",
       y = "Count",
       fill = "Gender Group",
       caption = "Figure 1.2")+
  scale_x_continuous(breaks = c(2012, 2016), labels = c("2012", "2016")) + # Set x-axis breaks
  theme_minimal()


###Year by Age###
ggplot(dat02, aes(x = year, fill = age_cat)) +
  geom_bar(position = "dodge", stat="count") +
  labs(title = "Count of People in Each Age Group by Year",
       x = "Age Group",
       y = "Count",
       fill = "Age Group",
       caption = "Figure 1.3")+
  scale_x_continuous(breaks = c(2012, 2016), labels = c("2012", "2016")) + # Set x-axis breaks
  theme_minimal()

###Year by Household Income###
ggplot(dat02, aes(x = year, fill = income_cat)) +
  geom_bar(position = "dodge", stat="count") +
  labs(title = "Count of People in Each Household income Group by Year",
       x = "Household Income Group",
       y = "Count",
       fill = "Household Income Group",
       caption = "Figure 1.4")+
  scale_x_continuous(breaks = c(2012, 2016), labels = c("2012", "2016")) + # Set x-axis breaks
  theme_minimal()

###Year by Religion###
ggplot(dat02, aes(x = year, fill = religion_cat)) +
  geom_bar(position = "dodge", stat="count") +
  labs(title = "Count of People in Each Religion Group by Year",
       x = "Religion Group",
       y = "Count",
       fill = "Religion Group",
       caption = "Figure 1.5")+
  scale_x_continuous(breaks = c(2012, 2016), labels = c("2012", "2016")) + # Set x-axis breaks
  theme_minimal()

###Year by Education###
ggplot(dat02, aes(x = year, fill = education_cat)) +
  geom_bar(position = "dodge", stat="count") +
  labs(title = "Count of People in Educational attainment  by Year",
       x = "Educational Attainment",
       y = "Count",
       fill = "Educational Attainment",
       caption = "Figure 1.6")+
  scale_x_continuous(breaks = c(2012, 2016), labels = c("2012", "2016")) + # Set x-axis breaks
  theme_minimal()

###Year by Party###
ggplot(dat02, aes(x = year, fill = party_cat)) +
  geom_bar(position = "dodge", stat="count") +
  labs(title = "Count of People in Party Affiliation by Year",
       x = "Party Affiliation",
       y = "Count",
       fill = "Party Affiliation",
       caption = "Figure 1.7")+
  scale_x_continuous(breaks = c(2012, 2016), labels = c("2012", "2016")) + # Set x-axis breaks
  theme_minimal()

## Summary Statistics Table####
library(vtable)
summarydat01<-dat2012%>%select(race_cat,candidate_cat,income_cat,religion_cat,education_cat,party_cat,gender_cat,age_cat)
sumtable(summarydat01)
summarydat02<-dat2016%>%select(race_cat,candidate_cat,income_cat,religion_cat,education_cat,party_cat,gender_cat,age_cat)
sumtable(summarydat02)
sumtable(summarydat)




## Task2
## binomial model building
##changing reference category
dat02$candidate_cat<-relevel(dat02$candidate_cat,ref="Democrat")
dat02$race_cat<-relevel(dat02$race_cat,ref="White")
dat02$income_cat<-relevel(dat02$income_cat,ref="lower income")
dat02$religion_cat<-relevel(dat02$religion_cat,ref="Protestant")
dat02$gender_cat<-relevel(dat02$gender_cat,ref="Female")
dat02$education_cat<-relevel(dat02$education_cat,ref="Below HS")
dat02$south_cat<-relevel(dat02$south_cat,ref="South")

dat2012<-dat02 %>% filter (year==2012)
dat2016 <- dat02 %>% filter (year==2016)


levels(dat02$race_cat)
## 2012 model####

mod.1 <- glm(candidate_cat ~ age_cat + race_cat + gender_cat+
                income_cat + religion_cat + south_cat +
               education_cat + party_cat, 
             data = dat2012, 
             na.action = na.exclude, 
             family = binomial("logit"))

summary(mod.1)


mod.2 <- glm(candidate_cat ~ age_cat + race_cat +
               income_cat + religion_cat + south_cat +
               education_cat + party_cat, 
             data = dat2012, 
             na.action = na.exclude, 
             family = binomial("logit"))

summary(mod.2)

library(lmtest)
AIC(mod.1)
AIC(mod.2)
lrtest(mod.1, mod.2)
logLik(mod.1)
logLik(mod.2)

## 2016 Model####

mod.3 <- glm(candidate_cat ~ age_cat + race_cat + gender_cat+
               income_cat + religion_cat + south_cat +
               education_cat + party_cat, 
             data = dat2016, 
             na.action = na.exclude, 
             family = binomial("logit"))

summary(mod.3)


mod.4 <- glm(candidate_cat ~ age_cat + race_cat +
                religion_cat + south_cat +
               education_cat + party_cat, 
             data = dat2016, 
             na.action = na.exclude, 
             family = binomial("logit"))

summary(mod.4)


AIC(mod.3)
AIC(mod.4)
lrtest(mod.3, mod.4)
logLik(mod.3)
logLik(mod.4)

##Task2####
trip.data <- read.csv("trip_data.csv")
person.data <- read.csv("person_data.csv")
household.data <- read.csv("household_data.csv")

trip.data<-read.csv("C:\\Users\\USER\\Desktop\\PlanningByNumbers\\Assignment03\\publicdb_release\\DVRPC HTS Database Files\\4_Trip_Public.csv")
person.data<-read.csv("C:\\Users\\USER\\Desktop\\PlanningByNumbers\\Assignment03\\publicdb_release\\DVRPC HTS Database Files\\2_Person_Public.csv")
Household_public<-read.csv("C:\\Users\\USER\\Desktop\\PlanningByNumbers\\Assignment03\\publicdb_release\\DVRPC HTS Database Files\\1_Household_Public.csv")

#filtering, recategorizing, renaming, and selecting variables#### 
#take trip dataset
#step 1. choose bike, auto, and transit trips that ended at work locations
trip.data <- trip.data %>%
  filter(MODE_AGG == 2 | MODE_AGG == 3 | MODE_AGG == 5) %>%
  filter(D_LOC_TYPE == 2)

#step 2. recode modes to bike, car, and transit (please name the modes as such)
trip.data <- trip.data %>%
  mutate(mode_type = recode(MODE_AGG,
                            `2` = "Bike",
                            `3` = "Car",
                            `5` = "Transit"))

#step 3. some people took multiple work trips. for each person, keep only the first work trip
trip.data <- trip.data %>%
  group_by(PERSON_ID, TRIP_NUM)
#step 4. select household id, person id, parking costs, travel time (model simulated), and travel distance (model simulated)

#take person dataset
#step 1. identify personal variables that might be associated with mode choice (be careful about the 988 value for race)
#step 2. for each variable, remove meaningless values
#step 3. recode values to more sensible categories
#step 4. select only the relevant variables

#take household dataset
#step 1. identify household variables that might be associated with mode choice
#step 2. for each variable, remove meaningless values
#step 3. recode values to more sensible categories
#step 4. select only the relevant variables

#joining trip dataset to person dataset
#step 1. join trip, person, and household datasets
#step 2. examine variables, remove outliers (let's keep only people who travel < 10 miles, < 120 minutes, and paid <$50 for parking)
#step 3. remove NAs. if this step leaves you with a few observations (say, a few hundred), then inspect variables to see
#if certain variable has lots of NAs and whether it would be reasonable to remove that variable altogether in order to
#preserve sample size

#we will do the following in class####
#calculating average speed for each mode
ave.speed <- dat %>% group_by(mode_cat) %>%
  summarise(ave_speed = mean(travel_dist/(travel_time/60)))

#calculating travel time for alternative modes
#need to do it one mode at a time
dat.car <- dat %>% filter(mode_cat == "car")
dat.transit <- dat %>% filter(mode_cat == "transit")
dat.bike <- dat %>% filter(mode_cat == "bike")

## calculating potential travel time for each alternative mode of transit
dat.car$time.car <- dat.car$travel_time
dat.car$time.transit <- 60*(dat.car$travel_dist/ave.speed$ave_speed[2]) + 10 #add 10 minutes for waiting and walking to station
dat.car$time.bike <- 60*(dat.car$travel_dist/ave.speed$ave_speed[3])

dat.transit$time.transit <- dat.transit$travel_time + 10
dat.transit$time.car <- 60*(dat.transit$travel_dist/ave.speed$ave_speed[1])
dat.transit$time.bike <- 60*(dat.transit$travel_dist/ave.speed$ave_speed[3])

dat.bike$time.bike <- dat.bike$travel_time
dat.bike$time.car <- 60*(dat.bike$travel_dist/ave.speed$ave_speed[1])
dat.bike$time.transit <- 60*(dat.bike$travel_dist/ave.speed$ave_speed[2]) + 10

#an overwhelming number of people drove
#let's take a subset so that the mode split is more balanced
set.seed(seed = 100)
rows <- sample(1:nrow(dat.car), 500)
dat.car <- dat.car[rows,]

dat <- rbind.data.frame(dat.car, dat.transit, dat.bike)

#calculating travel costs for alternative modes
#Dept. of Energy est. $0.6/mile for driving
dat$cost.car <- dat$travel_dist * 0.6 #+ dat$parking_cost #leave parking cost out for now

#multiply 0.5 for non-monetary costs, add $2 for ticket
dat$cost.transit <- dat$travel_dist * 0.5 + 2

#add $3 for non-monetary cost and wear and tear
#Logan et al. (2023) #https://www.ncbi.nlm.nih.gov/pmc/articles/PMC10546027/
dat$cost.bike <- dat$travel_dist + 3 

#shaping data into correct format
library(mlogit)
dat.logit <- mlogit.data(dat, shape="wide", 
                         choice="mode_cat", 
                         varying=c(22:27)) 
#the 23:28 are column numbers of the alternative specific variables we created
#your column numbers might not be the same as mine

#notice the time and cost variables? we did not create them, 
#but mlogit was able to figure it out based on the naming
#scheme we used when creating the time.mode and cost.mode variables

#estimating multinomial logit model
#formula specification
#(dependent variable ~ alt. specific with generic coef. | individual specific | alt. specific with alt. specific coef.)

names(dat)
#fit a simple one for now
mod.1 <- mlogit (mode_cat ~ cost + income | income_cat, data = dat.logit)
summary(mod.1)

#predicting
mode.prob <- data.frame(fitted(mod.1, outcome = FALSE))
dat <- cbind.data.frame(dat, mode.prob)

dat$pred_mode <- 0

#use actual share in observed as thresholds
table(dat$mode_agg)/length(dat$mode_agg)

for (i in 1:length(dat$hh_id)) {
  if (dat$car[i] > ) {
    dat$pred_mode[i] = "car"
  } else if (dat$transit[i] > ) {
    dat$pred_mode[i] = "transit"
  } else if (dat$bike[i] > ) {
    dat$pred_mode[i] = "bike"
  }
}

#do not use this for now because it predicts bike poorly
for (i in 1:length(dat$hh_id)) {
  if (dat$car[i] > dat$bike[i] & dat$car[i] > dat$transit[i]) {
    dat$pred_mode[i] = "car"
  } else if (dat$transit[i] > dat$car[i] & dat$transit[i] > dat$bike[i]) {
    dat$pred_mode[i] = "transit"
  } else if (dat$bike[i] > dat$car[i] & dat$bike[i] > dat$transit[i]) {
    dat$pred_mode[i] = "bike"
  }
}

#calculate model fit statistics####
#count R squared
#misclassification error
#correct rate for car
#correct rate for transit
#correct rate for bike

#how changing variables affect mode choice####
#a few options to think about
#what if we increase cost of driving by xx%? how would that affect predicted average probabilities?
#what if we reduce travel time on transit by xx%? how would that affect predicted average probabilities?
#what if we increase the income of those who are currently at the bottom of the income brackets by xx%?

#increasing driving cost by 50%
dat.1 <- dat
dat.1$cost.car <- dat.1$cost.car * 1.5
dat.1.logit <- mlogit.data(dat.1, shape="wide", 
                           choice="mode_cat", varying=c(23:28))  
mod.2 <- mlogit (mode_cat ~ cost + time | income_cat, data = dat.1.logit)

#average predicted probability for using each mode before and after intervention
fitted(mod.2, outcome = FALSE)

apply(fitted(mod.2, outcome=FALSE), 2, mean)
apply(fitted(mod.1, outcome=FALSE), 2, mean)

#this is what the function is doing
len <- length(dat.logit$hh_id)/3
mean(fitted(mod.2, outcome = FALSE)[1:]) #plug in the value of len [1:len]
mean(fitted(mod.2, outcome = FALSE)[:]) #plug in the value of len to calculate [(len + 1):len*2]
mean(fitted(mod.2, outcome = FALSE)[:]) #plug in the value of len to calculate [(len*2+1):len*3]

mean(fitted(mod.1, outcome = FALSE)[1:]) #same as above
mean(fitted(mod.1, outcome = FALSE)[:])
mean(fitted(mod.1, outcome = FALSE)[:])




