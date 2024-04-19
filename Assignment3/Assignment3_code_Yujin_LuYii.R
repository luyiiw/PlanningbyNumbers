#ASSIGNMENT 3####
rm(list = ls())
library(haven)
library(arm)
library(tidyverse)

#Question 1: Voting####
##Data loading
load("C:\\Users\\USER\\Desktop\\PlanningByNumbers\\Assignment03\\cpln505_assignment3_voting_data_abb.rda")
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
##Plot
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




#Question 2: Work Transit####
rm(list = ls())
library(tidyverse)

#Task 1: Selecting Variables####
#set working directory 
setwd("/Users/luyiiwong/Documents/Planning_by_numbers/Module3")


##Trip data####
#filtering, recategorizing, renaming, and selecting variables
#take trip dataset
trip.dat <- read.csv("trip_data.csv")

#yujin dataset
#trip.dat<-read.csv("C:\\Users\\USER\\Desktop\\PlanningByNumbers\\Assignment03\\publicdb_release\\DVRPC HTS Database Files\\4_Trip_Public.csv")
#person.dat<-read.csv("C:\\Users\\USER\\Desktop\\PlanningByNumbers\\Assignment03\\publicdb_release\\DVRPC HTS Database Files\\2_Person_Public.csv")
#household.dat<-read.csv("C:\\Users\\USER\\Desktop\\PlanningByNumbers\\Assignment03\\publicdb_release\\DVRPC HTS Database Files\\1_Household_Public.csv")

#step 1. choose bike, auto, and transit trips that ended at work locations
trip.dat <- trip.dat %>%
  filter(MODE_AGG %in% c(2, 3, 5) & D_LOC_TYPE == 2)

## MODE_AGG = 2,3,5
## D_LOC_TYPE = 2 (WORK)

#step 2. recode modes to bike, car, and transit (please name the modes as such)
trip.dat <- trip.dat %>%
  mutate(transit_mode = as.factor(
    recode(MODE_AGG,
           `2` = "bike",
           `3` = "car",
           `5` = "transit")))

#step 3. some people took multiple work trips. for each person, keep only the first work trip

trip.dat <- trip.dat %>%
  filter(TRIP_NUM == 1)

#step 4. select household id, person id, parking costs, travel time (model simulated), and travel distance (model simulated)

trip.dat <- trip.dat %>%
  select(HH_ID, PERSON_ID, Model_TravTime, Model_TravDist, transit_mode) %>%
  rename(travel_time = Model_TravTime,
         travel_dist= Model_TravDist)

##Person data####  
#take person dataset
person.dat <- read.csv("person_data.csv")

#step 1. identify personal variables that might be associated with mode choice (be careful about the 988 value for race)
person.dat <- person.dat %>%
  select(HH_ID, PERSON_ID, EDUCA, LIC, PARK_SUB, TRAN_SUB)

#step 2. for each variable, remove meaningless values
## removing NA rows
person.dat <- na.omit(person.dat)

## removing education refused or don't know rows
person.dat <- person.dat[!(person.dat$EDUCA %in% "99"),]
person.dat <- person.dat[!(person.dat$EDUCA %in% "98"),]

## removing people with unknown license
person.dat <- person.dat[!(person.dat$LIC %in% "99"),]
person.dat <- person.dat[!(person.dat$LIC %in% "98"),]

## removing don't know and refused answer for PARK_SUB
person.dat <- person.dat[!(person.dat$PARK_SUB %in% "8"),]
person.dat <- person.dat[!(person.dat$PARK_SUB %in% "9"),]

## removing don't know and refused answer for TRAN_SUB
person.dat <- person.dat[!(person.dat$TRAN_SUB %in% "8"),]

#step 3. recode values to more sensible categories

person.dat <- person.dat %>%
  mutate(education = as.factor(
    recode(EDUCA,
           `1` = "Below HS",
           `2` = "HS Grad",
           `3` = "Some College",
           `4` = "Associate Degree",
           `5` = "BS Degree",
           `6` = "Graduate Degree",
           `97` = "Other"))) %>%
  mutate(driver_license = as.factor(
    recode(LIC,
           `1` = "Yes",
           `2` = "No"))) %>%
  mutate(parking_subsidy = as.factor(
    recode(PARK_SUB,
           `1` = "Subsidized Parking",
           `2` = "No Parking Subsidy",
           `3` = "Free Parking"))) %>%
  mutate(transit_subsidy = as.factor(
    recode(TRAN_SUB,
           `1` = "Subsidized Transit",
           `2` = "No Transit Subsidy")))

#step 4. select only the relevant variables
person.dat <- person.dat %>%
  select(HH_ID, PERSON_ID, education, driver_license, parking_subsidy, transit_subsidy)

##Household data####
#take household dataset
household.dat <- read_csv("household_data.csv")

#step 1. identify household variables that might be associated with mode choice
household.dat <- household.dat %>%
  select(HH_ID, H_COUNTY, A_TYPE, HH_SIZE, TOT_VEH, TOLL_ACCNT, INCOME)

#step 2. for each variable, remove meaningless values
## removing NA rows
household.dat <- na.omit(household.dat)

## removing TOLL_ACCNT don't know rows
household.dat <- household.dat[!(household.dat$TOLL_ACCNT %in% "98"),]

## removing INCOME don't know or refused rows
household.dat <- household.dat[!(household.dat$INCOME %in% "98"),]
household.dat <- household.dat[!(household.dat$INCOME %in% "99"),]

#step 3. recode values to more sensible categories
household.dat <- household.dat %>%
  mutate(county = as.factor(
    recode(H_COUNTY,
           `42017` = "Bucks",
           `42029` = "Chester",
           `42045` = "Delaware",
           `42091` = "Montgomery",
           `42101` = "Philadelphia",
           `34005` = "Burlington",
           `34007` = "Camden",
           `34015` = "Gloucester",
           `34021` = "Mercer"))) %>%
  mutate(area_type = as.factor(
    recode(A_TYPE,
           `1` = "Urban",
           `2` = "Urban",
           `3` = "Urban",
           `4` = "Suburban",
           `5` = "Rural",
           `6` = "Rural")))

## renaming columns 
household.dat <- household.dat %>%
  rename(household_size = HH_SIZE) %>%
  rename(total_veh = TOT_VEH) %>%
  rename(toll_account = TOLL_ACCNT)

## re categorizing income
household.dat <- household.dat %>%
  mutate(hh_income = case_when(
    INCOME == '1' ~ 'low_income',
    INCOME == '2' ~ 'low_income',
    INCOME == '3' ~ 'low_income',
    INCOME == '4' ~ 'low_income',
    INCOME == '5' ~ 'middle_income',
    INCOME == '6' ~ 'middle_income',
    INCOME == '7' ~ 'middle_income',
    INCOME >= '8' ~ 'high_income'))

#step 4. select only the relevant variables
household.dat <- household.dat %>%
  select(-H_COUNTY, -A_TYPE, -INCOME)

##Joining data sets####
#joining trip dataset to person dataset
#step 1. join trip, person, and household datasets
dat <- merge(trip.dat, person.dat, by = "HH_ID", all.x = FALSE, all.y=FALSE, sort = FALSE) 
dat <- merge(household.dat, dat, by = "HH_ID", all.x = FALSE, all.y=FALSE, sort = FALSE)

#step 2. examine variables, remove outliers (let's keep only people who travel < 10 miles, < 120 minutes, and paid <$50 for parking)
dat <- dat %>%
  filter(travel_dist < 10,
         travel_time < 120)

#step 3. remove NAs. if this step leaves you with a few observations (say, a few hundred), then inspect variables to see
#if certain variable has lots of NAs and whether it would be reasonable to remove that variable altogether in order to
#preserve sample size
any(is.na(dat))
sum(is.na(dat))

## removing NA rows
dat <- na.omit(dat)

#calculating average speed for each mode
ave.speed <- dat %>% group_by(transit_mode) %>%
  summarise(ave_speed = mean(travel_dist/(travel_time/60)))

#calculating travel time for alternative modes
#need to do it one mode at a time
dat.car <- dat %>% filter(transit_mode == "car")
dat.transit <- dat %>% filter(transit_mode == "transit")
dat.bike <- dat %>% filter(transit_mode == "bike")


## bike=1, car=2, transit=3
dat.car$time.car <- dat.car$travel_time
dat.car$time.transit <- 60*(dat.car$travel_dist/ave.speed$ave_speed[3]) + 10 #add 10 minutes for waiting and walking to station
dat.car$time.bike <- 60*(dat.car$travel_dist/ave.speed$ave_speed[1])

dat.transit$time.transit <- dat.transit$travel_time + 10
dat.transit$time.car <- 60*(dat.transit$travel_dist/ave.speed$ave_speed[2])
dat.transit$time.bike <- 60*(dat.transit$travel_dist/ave.speed$ave_speed[1])

dat.bike$time.bike <- dat.bike$travel_time
dat.bike$time.car <- 60*(dat.bike$travel_dist/ave.speed$ave_speed[2])
dat.bike$time.transit <- 60*(dat.bike$travel_dist/ave.speed$ave_speed[3]) + 10

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
                      choice="transit_mode", 
                      varying=c(17:22)) 
summary(dat.logit)
#the 17:22 are column numbers of the alternative specific variables we created


#Task 2: Exploratory Data Analysis ####
# make sure that the variables have the correct data type

dat$HH_ID <- as.character(dat$HH_ID)
dat$PERSON_ID.x <- as.character(dat$PERSON_ID.x)
dat$PERSON_ID.y <- as.character(dat$PERSON_ID.y)  

dat$hh_income <- as.factor(dat$hh_income)

## Summary Statistics Table####
library(vtable)
sumtable(dat)
# should create a table with the variables that we use for the final model 
#### Continuous variables ####
final.var.cont <- dat %>%
  select(household_size, total_veh, travel_time, travel_dist, time.car, time.transit, time.bike, cost.car,
         cost.transit, cost.bike)

sumtable(final.var.cont)

#### Categorical variables ####
# remove county (even though it is used in the model)
final.var.cat <- dat %>%
  select(area_type, hh_income, transit_mode, driver_license, parking_subsidy)

sumtable(final.var.cat)

## Charts for Categorical Variables####
library(ggplot2)
library(dplyr)

### Final stat variables for the model ####
# transit color palette
t.color <- c("#E1BEE7", "#AB47BC", "#6A1B9A")

### Transit mode by income group ####
ggplot(cat.dat, aes(x = hh_income, fill = transit_mode)) +
  geom_bar(position = "dodge") +
  labs(title = "Count of People in Each\nIncome Group by Transit Mode",
       x = "Income Group",
       y = "Count",
       fill = "Transit Mode",
       caption = "Figure 2.1")  +
  scale_fill_manual(values = t.color) +
  theme_minimal()

### Transit mode by area type ####
ggplot(cat.dat, aes(x = area_type, fill = transit_mode)) +
  geom_bar(position = position_dodge()) +
  labs(title = "Count of People in Area Type by Transit Mode",
       x = "Area Type",
       y = "Count",
       fill = "Transit Mode",
       caption = "Figure 2.2")  +
  scale_fill_manual(values = t.color) +
  theme_minimal()

### Transit mode by county####
ggplot(cat.dat, aes(x = county, fill = transit_mode)) +
  geom_bar(position = "dodge") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(title = "Count of People's Education Attainment \nType by Transit Mode",
       x = "County",
       y = "Count",
       fill = "Transit Mode",
       caption = "Figure 2.3")  +
  scale_fill_manual(values = t.color) +
  theme_minimal()

### Transit mode by parking subsidy ####
ggplot(cat.dat, aes(x = parking_subsidy, fill = transit_mode)) +
  geom_bar(position = position_dodge()) +
  labs(title = "Count of Transit Mode by Levels of Parking Costs",
       x = "Parking Cost",
       y = "Count",
       fill = "Transit Mode",
       caption = "Figure 2.4")  +
  scale_fill_manual(values = t.color) +
  theme_minimal()


#Task 3: Multinomial Logit Model####
#estimating multinomial logit model
#formula specification
#(dependent variable ~ alt. specific with generic coef. | individual specific | alt. specific with alt. specific coef.)
names(dat)

# write theory

# setting reference levels for relevant variables: hh_income 
# make hh_income a factor variable to re-level
dat.logit$hh_income <- factor(dat.logit$hh_income)
dat.logit$hh_income <- relevel(dat.logit$hh_income, ref = "low_income")

## Test Model####
#test model includes all variables that we selected prior to data cleaning 
mod.test <-  mlogit (transit_mode ~ cost + time | household_size + total_veh + toll_account + county +
                       area_type + hh_income + education + driver_license  + parking_subsidy +
                       transit_subsidy
                     , data = dat.logit, reflevel = "car")
summary(mod.test)

## Final Model####
# We removed insignificant variables until no variables could be removed
mod.final <-  mlogit (transit_mode ~ cost + time | household_size + total_veh + county + area_type
                  + hh_income + driver_license + parking_subsidy
                     , data = dat.logit, reflevel = "car")

summary(mod.final)

## Creating predictions####
#creating a new data frame to include predictions
mode.prob <- data.frame(fitted(mod.final, outcome = FALSE))
dat.pred <- cbind.data.frame(dat, mode.prob)
dat.pred$pred_mode <- 0

summary(dat.pred)

#populating predictions using actual share in observed as thresholds
table(dat.pred$transit_mode)/length(dat.pred$transit_mode)

for (i in 1:nrow(dat.pred)) {
  if (dat.pred$car[i] > 0.65530799) {
    dat.pred$pred_mode[i] = "car"
  } else if (dat.pred$transit[i] > 0.27653997) {
    dat.pred$pred_mode[i] = "transit"
  } else if (dat.pred$bike[i] > 0.06815203) {
    dat.pred$pred_mode[i] = "bike"
  }
}

### Calculate model fit statistics####
AIC(mod.test)
AIC(mod.final)
lrtest(mod.test, mod.final)

# Task 4: Policy Interventions####
# Intervention 1: Implementing a road tax that would increase driving cost by 50%
dat.1 <- dat
dat.1$cost.car <- dat.1$cost.car * 1.5
dat.1.logit <- mlogit.data(dat.1, shape="wide", 
                           choice="transit_mode", varying=c(17:22))  

#average predicted probability for using each mode before and after intervention
apply(predict(mod.final, type = "response", newdata = dat.logit), 2, mean)
apply(predict(mod.final, type = "response", newdata = dat.1.logit), 2, mean)


# Intervention 2: Decreasing cost of public transit by 40%
dat.2 <- dat

dat.2$cost.transit <- dat.1$cost.transit * 0.6
dat.2.logit <- mlogit.data(dat.2, shape="wide", 
                           choice="transit_mode", varying=c(17:22))  
apply(predict(mod.final, type = "response", newdata = dat.logit), 2, mean)
apply(predict(mod.final, type = "response", newdata = dat.2.logit), 2, mean)

#Intervention 3: Increasing driving costs and reducing public transit costs 
dat.3 <- dat
dat.3$cost.car <- dat.3$cost.car * 1.5
dat.3$cost.transit <- dat.3$cost.transit * 0.6
dat.3.logit <- mlogit.data(dat.3, shape="wide", 
                           choice="transit_mode", varying=c(17:22))  
apply(predict(mod.final, type = "response", newdata = dat.logit), 2, mean)
apply(predict(mod.final, type = "response", newdata = dat.3.logit), 2, mean)

#unsuccessful#
#dat.2$parking_subsidy <- recode_factor(dat.2$parking_subsidy,
#                                       `Free Parking` = "No Parking Subsidy",
#                                       `Subsidized Parking` = "Subsidized Parking",
#                                       `No Parking Subsidy` = "No Parking Subsidy")
#levels(dat.2$parking_subsidy)


# Task 5: Model Performance####
#count R squared - percent of observations correctly predicted by the model
library(nnet)

table(predicted = dat.pred$pred_mode, 
      observed = dat.pred$transit_mode)

## Confusion Matrix ####
prop.table(table(predicted = dat.pred$pred_mode, 
                 observed = dat.pred$transit_mode), margin = 2)

#misclassification error: 18.6%
#correct rate for car: 88.8%
#correct rate for transit: 86.7%
#correct rate for bike: 1.92%


