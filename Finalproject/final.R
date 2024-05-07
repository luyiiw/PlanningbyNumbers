library(tidycensus)
library(tidyverse)
library(viridis)
library(ggplot2)
library(sf)
library(mapview)
library(riem)

# Setup ####
setwd("/Users/luyiiwong/Documents/GitHub/PlanningbyNumbers/Finalproject")

## Load Themes. ####
plotTheme <- theme(
  plot.title =element_text(size=12),
  plot.subtitle = element_text(size=8),
  plot.caption = element_text(size = 6),
  axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
  axis.text.y = element_text(size = 8),
  axis.title.y = element_text(size = 8),
  axis.title.x = element_text(size = 8),
  # Set the entire chart region to blank
  panel.background=element_blank(),
  plot.background=element_blank(),
  #panel.border=element_rect(colour="#F0F0F0"),
  # Format the grid
  panel.grid.major=element_line(colour="#D0D0D0",size=.2),
  axis.ticks=element_blank())

mapTheme <- theme(plot.title =element_text(size=12),
                  plot.subtitle = element_text(size=8),
                  plot.caption = element_text(size = 6),
                  axis.line=element_blank(),
                  axis.text.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  panel.background=element_blank(),
                  panel.border=element_blank(),
                  panel.grid.major=element_line(colour = 'transparent'),
                  panel.grid.minor=element_blank(),
                  legend.direction = "vertical", 
                  legend.position = "right",
                  plot.margin = margin(1, 1, 1, 1, 'cm'),
                  legend.key.height = unit(1, "cm"), legend.key.width = unit(0.2, "cm"))


## Load Palette ####
palette12 <- c("#B3C1F5","#869BF5","#506BD4","#1C48A4","#1A2299","#A2D4B7","#24A65A","#24A686","#2D695A","#b59bed","#7742C7","#4114A2")
palette10 <- c("#B3C1F5","#869BF5","#506BD4","#1C48A4","#A2D4B7","#24A65A","#24A686","#2D695A","#b59bed","#7742C7")
palette5 <- c("#B3C1F5","#869BF5","#506BD4","#1C48A4","#1A2299")
palette5b <- c("#4114A2","#b59bed","#A2D4B7","#24A65A","#7742C7")
palette4 <- c("#B3C1F5","#869BF5","#506BD4","#1C48A4")
palette3 <- c("#B3C1F5","#869BF5","#1C48A4")
palette2 <- c("#B3C1F5","#1C48A4")

# Load Data ####
## Setting Up Api Key #### 
census_api_key("b83a23afee4a8ed0fa131e449869e6577b87151e", overwrite = TRUE, install = TRUE)

## Pulling Census Data for 2011 ####
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

## Read in csv data ####
calc_2011 <- read_csv("2011_calculated.csv") 

## Cleaning Data ####
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


# Pulling Census Data for 2021 ####
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

## Read in csv data ####
calc_2021 <- read.csv("2021_calculated.csv")

## Cleaning Data ####
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

# Combining Datasets ####
comb_2021 <- merge(dat_2021_clean, lac_2021_clean, by = "GEOID") %>%
  mutate(pop_dens = total_pop/totalarea) %>%
  select(GEOID, NAMELSAD, total_pop, totalarea, pop_dens, median_hh_income, 
         renter_share, owner_share, perc_vulnerable, total_vulnerable, sum_treecanopy, sum_naturalcover)


# Dependent Variable ####
## Read in csv data 2011 weather ####
temp_2011 <- read.csv("PHL_maxtemp_2011.csv")

temp_2011 <- temp_2011 %>%
  mutate(Latitude = as.double(Latitude),
         Longitude = as.double(Longitude))

temp_2011 <- as.double(temp_2011)
temp_2011 <-as.double(temp_2011$Longitude)


mapview(temp_2011, xcol= "Logitude", ycol= "Latitude", crs = 4269, grid = FALSE)


heat_watch_2022 <- "r3dp9-datacite.json"

# Weather Feature Stations ####
# https://www.rdocumentation.org/packages/riem/versions/0.3.0/topics/riem_measures

weather.Panel.PHL <- 
  riem_measures(station = "PHL", date_start = "2011-07-23", date_end = "2011-07-24") %>%
  dplyr::select(valid, tmpf)%>%
  replace(is.na(.), 0) %>%
  mutate(interval60 = ymd_h(substr(valid,1,13))) %>%
  mutate(dotw = wday(interval60, label=TRUE)) %>%
  group_by(interval60) %>%
  summarize(Temperature.PHL = max(tmpf)) %>%
  mutate(Temperature.PHL = ifelse(Temperature.PHL == 0, 42, Temperature.PHL))

weather.Panel.PNE <- 
  riem_measures(station = "PNE", date_start = "2011-07-23", date_end = "2011-07-24") %>%
  dplyr::select(valid, tmpf)%>%
  replace(is.na(.), 0) %>%
  mutate(interval60 = ymd_h(substr(valid,1,13))) %>%
  mutate(dotw = wday(interval60, label=TRUE)) %>%
  group_by(interval60) %>%
  summarize(Temperature.PNE = max(tmpf)) %>%
  mutate(Temperature.PNE = ifelse(Temperature.PNE == 0, 42, Temperature.PNE))

weather.Panel.LOM <- 
  riem_measures(station = "LOM", date_start = "2011-07-23", date_end = "2011-07-24") %>%
  dplyr::select(valid, tmpf)%>%
  replace(is.na(.), 0) %>%
  mutate(interval60 = ymd_h(substr(valid,1,13))) %>%
  mutate(dotw = wday(interval60, label=TRUE)) %>%
  group_by(interval60) %>%
  summarize(Temperature.LOM = max(tmpf)) %>%
  mutate(Temperature.LOM = ifelse(Temperature.LOM == 0, 42, Temperature.LOM))

## Combine weather data ####
comb_temp_2011 <- merge(weather.Panel.LOM, weather.Panel.PHL, by = "interval60")
comb_temp_2011 <- merge(comb_temp_2011, weather.Panel.PNE, by = "interval60")



# 2021 Regression ####
# read in lst data
lst_2021 <- read.csv("2021_lst_median.csv")

comb_2021 <- merge(comb_2021, lst_2021, by = "GEOID") %>%
  rename(temp = MEDIAN)

# Regression
mod.1 <- lm(temp ~ median_hh_income + renter_share + perc_vulnerable + sum_treecanopy +
              sum_naturalcover, data = comb_2021)

summary(mod.1)


## Exploratory Analysis - Independent variables
## dropping NA values first 
comb_2021_filtered <- na.omit(comb_2021)

# Median Household Income
ggplot() +
  geom_sf(data = comb_2021_filtered, 
          aes(fill = median_hh_income)) +
  scale_fill_viridis(option = "A", direction = -1) +
  labs(title = "Median Household Income by Census Tracts") +
  mapTheme

#Percentage Vulnerable
ggplot() +
  geom_sf(data = comb_2021_filtered, 
          aes(fill = perc_vulnerable), na.rm = TRUE) +
  scale_fill_viridis(option = "A", direction = -1) +
  labs(title = "Vulnerability Share by Census Tracts") +
  mapTheme

#Population Density
## consider plotting by category?
ggplot() +
  geom_sf(data = comb_2021, 
          aes(fill = pop_dens)) +
  scale_fill_viridis(option = "A", direction = -1) +
  labs(title = "Population Density by Census Tracts") +
  mapTheme

#Tree Canopy
ggplot() +
  geom_sf(data = comb_2021, 
          aes(fill = sum_treecanopy)) +
  scale_fill_viridis(option = "D") +
  labs(title = "Tree Canopy by Census Tracts") +
  mapTheme
