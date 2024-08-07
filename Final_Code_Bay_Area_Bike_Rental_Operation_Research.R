# COMPLETE SCRIPT

# 2. Perform an EDA on a copy of the data

# a) Install the relevant packages, i.e. tidyverse, funModeling, and Hmisc, etc.
library(tidyverse)
library(funModeling)
library(Hmisc)
library(dplyr)
library(lubridate)
library(stringr)
library(mctq)
library(tidyr)
library(ivs)
library(corrplot)
library(ggcorrplot)

# THE FOLLOWING EDA WILL EXAMINE STATION DATA, TRIP DATA, AND WEATHER DATA
# b) Examine data, make sure there is enough for suitable analysis, make sure everything is the right form

#load in the data in the csv file into a separate variable before examining the data
RawStation <- read.csv("station.csv")
summary(RawStation)
sum(as.character(RawStation$id) == "") #equals 0 as there are no missing values that aren't coded as NA
sum(as.character(RawStation$name) == "")
sum(as.character(RawStation$lat) == "")
sum(as.character(RawStation$long) == "")
sum(as.character(RawStation$dock_count) == "")
sum(as.character(RawStation$city) == "")
sum(as.character(RawStation$installation_date) == "")
#No missing values that aren't coded as NA

RawTrip <- read.csv("trip.csv")
summary(RawTrip)
sum(as.character(RawTrip$id) == "")
sum(as.character(RawTrip$duration) == "")
sum(as.character(RawTrip$start_date) == "")
sum(as.character(RawTrip$start_station_name) == "")
sum(as.character(RawTrip$start_station_id) == "")
sum(as.character(RawTrip$end_date) == "")
sum(as.character(RawTrip$end_station_name) == "")
sum(as.character(RawTrip$end_station_id) == "")
sum(as.character(RawTrip$bike_id) == "")
sum(as.character(RawTrip$subscription_type) == "")
#No missing values that aren't coded as NA

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

# c) Clean a copy of the data, mark missing values as NA, convert all data types into the proper format
CleanedStation <- RawStation
CleanedStation$id <- factor(CleanedStation$id)
CleanedStation$name <- factor(CleanedStation$name)
CleanedStation$city <- factor(CleanedStation$city)
CleanedStation$installation_date <- strptime(CleanedStation$installation_date, format = "%m/%d/%Y")

CleanedTrip <- RawTrip
CleanedTrip$id <- factor(CleanedTrip$id)
CleanedTrip$start_station_name <- factor(CleanedTrip$start_station_name)
CleanedTrip$start_station_id <- factor(CleanedTrip$start_station_id)
CleanedTrip$end_station_name <- factor(CleanedTrip$end_station_name)
CleanedTrip$end_station_id <- factor(CleanedTrip$end_station_id)
CleanedTrip$bike_id <- factor(CleanedTrip$bike_id)
CleanedTrip$subscription_type <- factor(CleanedTrip$subscription_type)
CleanedTrip$zip_code <- factor(CleanedTrip$zip_code)
CleanedTrip$start_date <- strptime(RawTrip$start_date, format = "%m/%d/%Y %H:%M")
CleanedTrip$end_date <- strptime(RawTrip$end_date, format = "%m/%d/%Y %H:%M")
summary(CleanedTrip)

CleanedWeather <- RawWeather
CleanedWeather$date <- strptime(CleanedWeather$date, format = "%m/%d/%Y") #convert the dates from strings to POSIX values for later processing
CleanedWeather$precipitation_inches[CleanedWeather$precipitation_inches=="T"]=0.001 # precipitation makes more sense as a numeric variable, and so trace values must be coded with a plausible numeric value
CleanedWeather$precipitation_inches <- as.numeric(CleanedWeather$precipitation_inches)
CleanedWeather$events[CleanedWeather$events==""]="Not Recorded"
CleanedWeather$events[CleanedWeather$events=="rain"]="Rain"
CleanedWeather$events <- factor(CleanedWeather$events)
CleanedWeather$city <- factor(CleanedWeather$city)

# d) Analyze categorical variables: do the categories make sense? Missingness?

# e) Analyze numerical variables: outliers? What is the distribution of data? What are the standard deviations?

# f) Analyze categorical and numerical variables at the same time: get a sense of all the variables to be analyzed

# 3. Record trip IDs for outliers, and then remove them. What are the rules for outliers? Outside of 95% of the data? Or a different metric?

# 4. Record and remove "cancelled trips" (i.e., any trip <3 minutes)

# 5. Determine weekday rush hours - What consists of rush hour? Provide a definition. Histograms by hour are a good idea to test, or 15 minutes to be more precise. Lubridate will be useful for determining time intervals

# 6. Determine 10 most frequent starting and ending stations during rush hours

# 7. Determine 10 most frequent starting and ending stations during the weekends

# 8. Determine the average utilization of bikes for each month (total time bikes were used/total time in the month)

# 9. Create a new dataset combining trip data with weather data. Weather data is available for each city and date. The cor function from the Corrplot package will be useful for creating a correlation matrix. Deal with the fact that it is difficult to correlate categorical and non-categorical variables. Some trips start and end in different cities, will need to explain how to correlate weather data with trip data if the trip is crossing cities - may have implications for how the two datasets are joined
