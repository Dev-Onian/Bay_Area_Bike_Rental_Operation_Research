# Weather EDA

# 1. Look at the data, get a sense of what it is like

# 2. Perform an EDA on a copy of the data
# a) Install tidyverse, funModeling, and Hmisc
library(tidyverse)
library(funModeling)
library(Hmisc)
library(dplyr)

# b) Examine data, make sure there is enough for suitable analysis, make sure everything is the right form

RawWeather <- read.csv("weather.csv")
summary(RawWeather)
sum(as.character(RawWeather$max_temperature_f) == "")
sum(as.character(RawWeather$mean_temperature_f) == "")
sum(as.character(RawWeather$min_temperature_f) == "")
sum(as.character(RawWeather$cloud_cover) == "")
# date is character instead of POSIX
# No missing values in the temperature variables, either as NAs or empty cells
# Need to examine visibility's units of miles, since there are visibility readings in excess of 4 miles, whereas human visibility on the ground is limited by the curvature of the Earth to only 3 miles. Note, this might be referring to laser visibility rather than geodesic visibility 
# precipitation_inches includes trace values, will set it to be 0.001 inches, which is lower than 0.01 inches

# c) Clean a copy of the data, including removing outliers - note what was changed
CleanedWeather <- RawWeather
CleanedWeather$date <- strptime(CleanedWeather$date, format = "%m/%d/%Y")
CleanedWeather$precipitation_inches[CleanedWeather$precipitation_inches=="T"]=0.001
CleanedWeather$precipitation_inches <- as.numeric(CleanedWeather$precipitation_inches)
CleanedWeather$events[CleanedWeather$events==""]="Not Recorded"
CleanedWeather$events[CleanedWeather$events=="rain"]="Rain"
CleanedWeather$events <- factor(CleanedWeather$events)
CleanedWeather$city <- factor(CleanedWeather$city)

# d) Analyze categorical variables: do the categories make sense? Missingness?
summary(CleanedWeather$events)
# high degree of missingness: 1473 Not Recorded - may indicate no weather events (sunny days are not coded for)
# Ways to impute missingness:
# - Fog: visibility less than 1 km
# - Mist: visibility between 1 km (0.62 mi) and 2 km (1.2 mi)
# - Haze from 2 km (1.2 mi) to 5 km (3.1 mi)
# - Temperatures at 14 F (-10°C) is too cold for the air to contain super-cooled 
#   water droplets so for forms small tiny ice crystals.
# - Rain: any amount of precipitation, even trace
# - Thunderstorm: hot temperatures, cloud cover, wind-speed, but highly variable

CleanedWeather <- mutate(CleanedWeather, ImputedEvents = case_when(
  precipitation_inches=0 & min_visibility_miles<0.62 ~ "Fog",
  precipitation_inches=0 & min_visibility_miles<1.2 & min_visibility_miles>0.62 ~ "Mist",
  precipitation_inches=0 & min_visibility_miles<3.1 & min_visibility_miles>1.2 ~ "Haze",
  precipitation_inches>0 & min_visibility_miles<0.62 ~ "Fog-Rain",
  precipitation_inches>0 & min_visibility_miles<1.2 & min_visibility_miles>0.62 ~ "Mist-Rain",
  precipitation_inches>0 & min_visibility_miles<3.1 & min_visibility_miles>1.2 ~ "Haze-Rain",
  precipitation_inches>0 & min_visibility_miles>3.1 ~ "Rain",
  .default = CleanedWeather$events
))
CleanedWeather$ImputedEvents <- factor(CleanedWeather$ImputedEvents)
summary(CleanedWeather$ImputedEvents)
#very limited efficacy for imputation, as too much information is missing. Still, 1423 events are missing - may indicate no weather events (sunny days are not coded for)

summary(CleanedWeather$city)
# no missing cities

# e) Analyze numerical variables: outliers? What is the distribution of data? What are the standard deviations?


# f) Analyze categorical and numerical variables at the same time: get a sense of all the variables to be analyzed

