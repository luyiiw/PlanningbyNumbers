#ASSIGNMENT 3####
# IN CLASS DEMO VER###
rm(list = ls())
library(haven)
library(arm)
library(tidyverse)

#VOTING####
load("cpln505_assignment3_voting_data_abb.rda")
load("/Users/luyiiwong/Documents/GitHub/PlanningbyNumbers/Assignment3/cpln505_assignment3_voting_data_abb.rda")

#Here is some data cleaning code to get you started
dat <- dat.voting %>% filter(VCF0004 == 2012 | VCF0004 == 2016) %>%
  rename(candidate = VCF0704a, income=VCF0114, year = VCF0004, religion=VCF0128, education=VCF0140a, class=VCF0148a, party=VCF0302, race = VCF0105a) %>%
  select(year, candidate, income, year, religion, education, class, party, race) %>%
  filter(race != 9, candidate != 0, education != 9, class !=9, !party %in% c(8, 9)) %>%
  mutate(
    race_cat = as.factor(recode(race,
                                     `1` = "white",
                                     `2` = "black",
                                     `3` = "Asian",
                                     `4` = "Indian",
                                     `5` = "Hispanic",
                                     `6` = "other")),
    candidate_cat=as.factor(recode(candidate,
                                   `1` = "Democrat",
                                   `2` = "Republican")),
    income_cat=as.factor(recode(income,
                              `1`="lower income",
                              `2`="lower income",
                              `3`="middle income",
                              `4`="high income",
                              `5`="high income")),
    religion_cat=as.factor(recode(religion,
                                  `1`="Protestant",
                                  `2`="Catholic",
                                  `3`="Jewish",
                                  `4`="Other")),
    education_cat=as.factor(recode(education,
                                   `1`=)))
`1` = "Below HS",
`2` = "Below HS",
`3` = "HS",
`4` = "Some College",
`5` = "BS Degree",
`6` = "Graduate Degree",
`97` = "Other")))





table(dat.voting$VCF0704a)
table(dat.voting$VCF01114)
table(dat.voting$VCF0128)
table(dat.voting$VCF0140a)
table(dat.voting$VCF0148a)
table(dat.voting$VCF0302)
table(dat.voting$VCF0105a)

#HHTS####
rm(list = ls())
library(tidyverse)

#set working directory 
setwd("/Users/luyiiwong/Documents/Planning_by_numbers/Module3")

#filtering, recategorizing, renaming, and selecting variables#### 
#take trip dataset
trip.dat <- read.csv("trip_data.csv")
#yujin dataset
trip.dat<-read.csv("C:\\Users\\USER\\Desktop\\PlanningByNumbers\\Assignment03\\publicdb_release\\DVRPC HTS Database Files\\4_Trip_Public.csv")
person.dat<-read.csv("C:\\Users\\USER\\Desktop\\PlanningByNumbers\\Assignment03\\publicdb_release\\DVRPC HTS Database Files\\2_Person_Public.csv")
household.dat<-read.csv("C:\\Users\\USER\\Desktop\\PlanningByNumbers\\Assignment03\\publicdb_release\\DVRPC HTS Database Files\\1_Household_Public.csv")
#step 1. choose bike, auto, and transit trips that ended at work locations
trip.dat <- trip.dat %>%
  filter(MODE_AGG %in% c(2, 3, 5) & D_LOC_TYPE == 2)

## MODE_AGG = 2,3,5
## D_LOC_TYPE = 2 (WORK)

#step 2. recode modes to bike, car, and transit (please name the modes as such)
trip.dat <- trip.dat %>%
  mutate(tranist_mode = as.factor(
    recode(MODE_AGG,
           `2` = "bike",
           `3` = "car",
           `5` = "transit")))

#step 3. some people took multiple work trips. for each person, keep only the first work trip

trip.dat <- trip.dat %>%
  filter(TRIP_NUM == 1)

#step 4. select household id, person id, parking costs, travel time (model simulated), and travel distance (model simulated)

trip.dat <- trip.dat %>%
  select(HH_ID, PERSON_ID, Model_TravTime, Model_TravDist)

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
  mutate(transit_mode = as.factor(
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
  select(HH_ID, PERSON_ID, education, driver_license, transit_mode, parking_subsidy, transit_subsidy)

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
           `4` = "Surburban",
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

#joining trip dataset to person dataset
#step 1. join trip, person, and household datasets
dat <- merge(trip.dat, person.dat, by = "HH_ID", all.x = FALSE, all.y=FALSE, sort = FALSE) 
dat <- merge(household.dat, dat, by = "HH_ID", all.x = FALSE, all.y=FALSE, sort = FALSE)
#step 2. examine variables, remove outliers (let's keep only people who travel < 10 miles, < 120 minutes, and paid <$50 for parking)
dat <- dat %>%
  filter(Model_TravDist < 10,
         Model_TravTime < 120)
#step 3. remove NAs. if this step leaves you with a few observations (say, a few hundred), then inspect variables to see
#if certain variable has lots of NAs and whether it would be reasonable to remove that variable altogether in order to
#preserve sample size
any(is.na(dat))
sum(is.na(dat))

#we will do the following in class####
#calculating average speed for each mode
ave.speed <- dat %>% group_by(transit_mode) %>%
  summarise(ave_speed = mean(Nodel_Traveldist/(travel_time/60)))

#calculating travel time for alternative modes
#need to do it one mode at a time
dat.car <- dat %>% filter(mode_cat == "car")
dat.transit <- dat %>% filter(mode_cat == "transit")
dat.bike <- dat %>% filter(mode_cat == "bike")

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
dat$cost.bike <- dat$travel_dist + 3 ###for the model experiment (question mark for this adding 3)

#shaping data into correct format
library(mlogit)
dat.logit <- mlogit.data(dat, shape="wide", 
                      choice="mode_cat", 
                      varying=c(22:27)) ##22:27 time.cost changes to time field
#the 22:27 are column numbers of the alternative specific variables we created
#your column numbers might not be the same as mine(it took a long time for summer!!)

#notice the time and cost variables? we did not create them, 
#but mlogit was able to figure it out based on the naming
#scheme we used when creating the time.mode and cost.mode variables

#estimating multinomial logit model
#formula specification
#(dependent variable ~ alt. specific with generic coef. | individual specific | alt. specific with alt. specific coef.)

names(dat)
#fit a simple one for now
mod.1 <- mlogit (mode_cat ~ cost + time | income_cat, data = dat.logit, reflevel="car", method="nr")
##if you think that time is also alt. specific with generic coef
summary(mod.1)

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
