#ASSIGNMENT 3####
rm(list = ls())
library(haven)
library(arm)
library(tidyverse)

#Question 1: Voting####
load("/Users/luyiiwong/Documents/GitHub/PlanningbyNumbers/Assignment3/cpln505_assignment3_voting_data_abb.rda")

#Here is some data cleaning code to get you started
dat <- dat.voting %>% filter(VCF0004 == 2012 | VCF0004 == 2016) %>%
  rename(candidate = VCF0704a, year = VCF0004, race = VCF0105a) %>%
  select(year, candidate, race) %>%
  filter(race != 9, candidate != 0) %>%
  mutate(race_cat = as.factor(recode(race,
                                     `1` = "white",
                                     `2` = "black",
                                     `3` = "other",
                                     `4` = "other",
                                     `5` = "other",
                                     `6` = "other")))


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
  select(HH_ID, PERSON_ID, EDUCA, LIC, WK_MODE, PARK_SUB, TRAN_SUB)

#step 2. for each variable, remove meaningless values
## removing NA rows
person.dat <- na.omit(person.dat)

## removing education refused or don't know rows
person.dat <- person.dat[!(person.dat$EDUCA %in% "99"),]
person.dat <- person.dat[!(person.dat$EDUCA %in% "98"),]

## removing people with unknown license
person.dat <- person.dat[!(person.dat$LIC %in% "99"),]
person.dat <- person.dat[!(person.dat$LIC %in% "98"),]

## removing odd data and refused answer for WORK_MODE
person.dat <- person.dat[!(person.dat$WK_MODE %in% "0"),]
person.dat <- person.dat[!(person.dat$WK_MODE %in% "9"),]

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
  mutate(work_mode = as.factor(
    recode(WK_MODE,
           `1` = "Car",
           `2` = "Carpool",
           `3` = "Car and Transit",
           `4` = "Tranist",
           `5` = "Bicylce",
           `6` = "Walking",
           `7` = "Other",
           `8` = "WFH"))) %>%
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
  select(HH_ID, PERSON_ID, education, driver_license, work_mode, parking_subsidy, transit_subsidy)

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
           `1` = "CBD",
           `2` = "CBD Fringe",
           `3` = "Urban",
           `4` = "Suburban",
           `5` = "Rural",
           `6` = "Open Rural")))

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
#any(is.na(dat))
#sum(is.na(dat))
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
                      varying=c(18:23)) 
summary(dat.logit)
#the 18:23 are column numbers of the alternative specific variables we created


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

cont.dat <- dat %>%
  select(household_size, total_veh, toll_account, travel_time, travel_dist, time.car, time.transit, time.bike, cost.car,
         cost.transit, cost.transit)

sumtable(cont.dat)

## Charts for Categorical Variables####
library(ggplot2)
library(dplyr)

cat.dat <- dat %>%
  select(county, area_type, hh_income, transit_mode, education, driver_license, work_mode, parking_subsidy, 
         transit_subsidy)

cat.dat <- cat.dat %>%
  group_by(transit_mode)

# transit color palette
t.color <- c("#E1BEE7", "#AB47BC", "#6A1B9A")

### transit mode by income group ####
ggplot(cat.dat, aes(x = hh_income, fill = transit_mode)) +
  geom_bar(position = "dodge") +
  labs(title = "Count of People in Each Income Group by Transit Mode",
       x = "Income Group",
       y = "Count",
       fill = "Transit Mode",
       caption = "Figure 2.1")  +
  scale_fill_manual(values = t.color) +
  theme_minimal()

### transit mode by area type ####
#re-categorize area type 
cat.dat <- cat.dat %>%
  mutate(a_type = case_when(
    area_type == 'Urban' ~ "Urban",
    area_type == 'CBD' ~ "Urban",
    area_type == 'CBD Fringe' ~ "Urban",
    area_type == 'Suburban' ~ "Suburban",
    area_type == 'Open Rural' ~ "Rural",
    area_type == 'Rural' ~ "Rural"))

ggplot(cat.dat, aes(x = a_type, fill = transit_mode)) +
  geom_bar(position = position_dodge()) +
  labs(title = "Count of People in Area Type by Transit Mode",
       x = "Area Type",
       y = "Count",
       fill = "Transit Mode",
       caption = "Figure 2.2")  +
  scale_fill_manual(values = t.color) +
  theme_minimal()

#transit mode by county
ggplot(cat.dat, aes(x = county, fill = transit_mode)) +
  geom_bar(position = "dodge") +
  labs(title = "Count of People's Education Attainment Type \n\ by Transit Mode",
       x = "County",
       y = "Count",
       fill = "Transit Mode",
       caption = "Figure 2.3")  +
  scale_fill_manual(values = t.color) +
  theme_minimal()

### transit mode by parking subsidy ####
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

#test model includes all variables that we selected prior to data cleaning 
mod.test <-  mlogit (transit_mode ~ cost + time | household_size + total_veh + toll_account + county +
                       area_type + hh_income + education + driver_license + work_mode + parking_subsidy +
                       transit_subsidy
                     , data = dat.logit, reflevel = "car")
summary(mod.test)


# retest
mod.1 <-  mlogit (transit_mode ~ cost + time | household_size + total_veh + county + area_type +
                    hh_income + driver_license + work_mode + parking_subsidy
                     , data = dat.logit, reflevel = "car")
summary(mod.1)

#predicting
mode.prob <- data.frame(fitted(mod.1, outcome = FALSE))
dat.1 <- cbind.data.frame(dat, mode.prob)

dat.1$pred_mode <- 0

#applying predicted transit mode for the whole data set based on probabilities
for (i in 1:length(dat.1$HH_ID)) {
  if (dat.1$car[i] > dat.1$bike[i] & dat.1$car[i] > dat.1$transit[i]) {
    dat.1$pred_mode[i] = "car"
  } else if (dat.1$transit[i] > dat.1$car[i] & dat.1$transit[i] > dat.1$bike[i]) {
    dat.1$pred_mode[i] = "transit"
  } else if (dat.1$bike[i] > dat.1$car[i] & dat.1$bike[i] > dat.1$transit[i]) {
    dat.1$pred_mode[i] = "bike"
  }
}

AIC(mod.test)
AIC(mod.1)
lrtest(mod.test, mod.1)

#####

# backward selection for final model
mod.final <- mlogit (transit_mode ~ cost + time | household_size +  hh_income + work_mode + total_veh
                    , data = dat.logit, reflevel = "car")
summary(mod.final)

#predicting
mode.prob <- data.frame(fitted(mod.final, outcome = FALSE))
dat <- cbind.data.frame(dat, mode.prob)

dat$pred_mode <- 0

#use actual share in observed as thresholds
#table(dat$mode_agg)/length(dat$mode_agg)

#applying predicted transit mode for the whole data set based on probabilities
for (i in 1:length(dat$HH_ID)) {
  if (dat$car[i] > dat$bike[i] & dat$car[i] > dat$transit[i]) {
    dat$pred_mode[i] = "car"
  } else if (dat$transit[i] > dat$car[i] & dat$transit[i] > dat$bike[i]) {
    dat$pred_mode[i] = "transit"
  } else if (dat$bike[i] > dat$car[i] & dat$bike[i] > dat$transit[i]) {
    dat$pred_mode[i] = "bike"
  }
}

#calculate model fit statistics####

AIC(mod.test)
AIC(mod.final)
lrtest(mod.test, mod.final)

# CHECK from here on####
table(mod.final$pred_mode)

#count R squared - percent of observations correctly predicted by the model
library(nnet)
summary(mod.final)

#CONFUSION MATRIX?
prop.table(table(predicted = stem.samp$pred_field, 
                 observed = stem.samp$job.cat), margin = 2)



table(observed = mod.final$transit_mode$job.cat, predicted = predict(object = mod.2))

prop.table(table(observed = mod.2$model$job.cat, 
                 predicted = predict(object = mod.2)), margin = 1)


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