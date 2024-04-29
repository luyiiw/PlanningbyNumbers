library(tidycensus)
library(tidyverse)
library(viridis)
library(ggplot2)
library(sf)

#setup 
setwd("/Users/luyiiwong/Documents/GitHub/PlanningbyNumbers/Finalproject")

#setting up api key 
census_api_key("b83a23afee4a8ed0fa131e449869e6577b87151e", overwrite = TRUE, install = TRUE)

#pulling data from census for 2011
dat_2011 <- get_acs(geography = "tract", 
          variables = c("B01003_001E",
                        "B01001_003E",
                        "B01001_004E",
                        "B01001_005E",
                        "B01001_024E",
                        "B01001_025E",
                        "B01001_027E",
                        "B01001_028E",
                        "B01001_029E",
                        "B01001_048E",
                        "B01001_049E",
                        "B19013_001E",
                        "B25003_003E",
                        "B25003_002E"), 
          year = 2011, 
          state = "PA", 
          county = "Philadelphia",
          geometry = TRUE, 
          output = "wide") %>%
  rename(total_pop = B01003_001E,
         male_under5 = B01001_003E,
         male_under10 = B01001_004E,
         male_under15 = B01001_005E,
         male_80 = B01001_024E,
         male_over80 =B01001_025E,
         female_under5 = B01001_027E,
         female_under10 = B01001_028E,
         female_under15 = B01001_029E,
         female_80 = B01001_048E,
         female_over80 = B01001_049E,
         median_hh_income = B19013_001E,
         renter_occupied = B25003_003E,
         owner_occupied = B25003_002E) %>%
  select(GEOID, NAME, total_pop, male_under5, male_under10, male_under15, male_80, male_over80, female_under5, female_under10, 
         female_under15, female_80, female_over80,
         median_hh_income, renter_occupied, owner_occupied) %>%
  mutate(vul_male = male_under5 + male_under10 + male_under15 + male_80 + male_over80,
         vul_female = female_under5 + female_under10 + female_under15 + female_80 + female_over80)

# read in csv data
calc_2011 <- read_csv("2011_calculated.csv") 

#cleaning data
lac_2011_clean <- calc_2011 %>%
  select(GEOID, NAMELSAD, totalarea, sum_treecanopy, sum_naturalcover)

dat_2011_clean <- dat_2011 %>%
  mutate(total_vulnerable = vul_female + vul_male,
         perc_vulnerable = total_vulnerable/total_pop,
         total_households = renter_occupied + owner_occupied,
         renter_share = renter_occupied/total_households,
         owner_share = owner_occupied/total_households) %>%
  select(GEOID, NAME, total_pop, median_hh_income, renter_share, owner_share, perc_vulnerable, total_vulnerable)

comb_2011 <- merge(dat_2011_clean, lac_2011_clean, by = "GEOID") %>%
  mutate(pop_dens = total_pop/totalarea) %>%
  select(GEOID, NAMELSAD, total_pop, totalarea, pop_dens, median_hh_income, 
         renter_share, owner_share, perc_vulnerable, total_vulnerable, sum_treecanopy, sum_naturalcover)


# pulling data from census for 2021
dat_2021 <- get_acs(geography = "tract", 
                    variables = c("B01003_001E",
                                  "B01001_003E",
                                  "B01001_004E",
                                  "B01001_005E",
                                  "B01001_024E",
                                  "B01001_025E",
                                  "B01001_027E",
                                  "B01001_028E",
                                  "B01001_029E",
                                  "B01001_048E",
                                  "B01001_049E",
                                  "B19013_001E",
                                  "B25003_003E",
                                  "B25003_002E"), 
                    year = 2021, 
                    state = "PA", 
                    county = "Philadelphia",
                    geometry = TRUE, 
                    output = "wide") %>%
  rename(total_pop = B01003_001E,
         male_under5 = B01001_003E,
         male_under10 = B01001_004E,
         male_under15 = B01001_005E,
         male_80 = B01001_024E,
         male_over80 =B01001_025E,
         female_under5 = B01001_027E,
         female_under10 = B01001_028E,
         female_under15 = B01001_029E,
         female_80 = B01001_048E,
         female_over80 = B01001_049E,
         median_hh_income = B19013_001E,
         renter_occupied = B25003_003E,
         owner_occupied = B25003_002E) %>%
  select(GEOID, NAME, total_pop, male_under5, male_under10, male_under15, male_80, male_over80, female_under5, female_under10, 
         female_under15, female_80, female_over80,
         median_hh_income, renter_occupied, owner_occupied) %>%
  mutate(vul_male = male_under5 + male_under10 + male_under15 + male_80 + male_over80,
         vul_female = female_under5 + female_under10 + female_under15 + female_80 + female_over80)

# read in csv data
calc_2021 <- read.csv("2021_calculated.csv")

#cleaning data
lac_2021_clean <- calc_2021 %>%
  select(GEOID, NAMELSAD, totalarea, sum_treecanopy, sum_naturalcover)

dat_2021_clean <- dat_2021 %>%
  mutate(total_vulnerable = vul_female + vul_male,
         perc_vulnerable = total_vulnerable/total_pop,
         total_households = renter_occupied + owner_occupied,
         renter_share = renter_occupied/total_households,
         owner_share = owner_occupied/total_households) %>%
  select(GEOID, NAME, total_pop, median_hh_income, renter_share, owner_share, 
         perc_vulnerable, total_vulnerable)

#combining datasets
comb_2021 <- merge(dat_2021_clean, lac_2021_clean, by = "GEOID") %>%
  mutate(pop_dens = total_pop/totalarea) %>%
  select(GEOID, NAMELSAD, total_pop, totalarea, pop_dens, median_hh_income, 
         renter_share, owner_share, perc_vulnerable, total_vulnerable, sum_treecanopy, sum_naturalcover)


## 2021 Regression
# read in lst data
lst_2021 <- read.csv("2021_lst_median.csv")

comb_2021 <- merge(comb_2021, lst_2021, by = "GEOID") %>%
  rename(temp = MEDIAN)

# Regression
mod.1 <- lm(temp ~ median_hh_income + renter_share + perc_vulnerable + sum_treecanopy +
              sum_naturalcover, data = comb_2021)

summary(mod.1)


## Exploratory Analysis - Independent variables
# Median Household Income 
ggplot() +
  geom_sf(data = comb_2021, 
          aes(fill = median_hh_income)) +
  scale_fill_viridis(option = "A") +
  labs(title = "Median Household Income by Census Tracts") +
  theme_bw()

#Percentage Vulnerable
ggplot() +
  geom_sf(data = comb_2021, 
          aes(fill = perc_vulnerable)) +
  scale_fill_viridis(option = "A") +
  labs(title = "Vulnerability Share by Census Tracts") +
  theme_minimal()

#Population Density
## consider plotting by category?
ggplot() +
  geom_sf(data = comb_2021, 
          aes(fill = pop_dens)) +
  scale_fill_viridis(option = "A") +
  labs(title = "Vulnerability Share by Census Tracts") +
  theme_minimal()

#Tree Canopy
ggplot() +
  geom_sf(data = comb_2021, 
          aes(fill = sum_treecanopy)) +
  scale_fill_viridis(option = "A") +
  labs(title = "Tree Canopy by Census Tracts") +
  theme_minimal()
