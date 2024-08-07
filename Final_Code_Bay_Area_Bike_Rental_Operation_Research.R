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

# Summarize the categorical variables to get a sense of the number of NAs and the distribution of values, as well as any values that represent the same thing but are coded differently
summary(CleanedStation$id)
summary(CleanedStation$name)
summary(CleanedStation$city)
summary(CleanedStation$installation_date)
# no missingness in the station categorical variables

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
summary(CleanedWeather$ImputedEvents) #the imputed events account for some missingness
#very limited efficacy for imputation, as too much information is missing. Still, 1423 events are missing - may indicate no weather events (sunny days are not coded for)
plot(CleanedWeather$ImputedEvents)

summary(CleanedWeather$city)
# no missing cities, equal distribution
plot(CleanedWeather$city)

summary(CleanedWeather$date)
# no times, only dates

# e) Analyze numerical variables: outliers? What is the distribution of data? What are the standard deviations?
summary(CleanedStation$lat)
summary(CleanedStation$long)
summary(CleanedStation$dock_count)
ggplot(CleanedStation, mapping = aes(x=long, y=lat, col = dock_count)) + geom_point() + scale_colour_gradient()
# This maps out the stations by latitude and longitude, colour-coded by their number of docks
# It doesn't make sense to remove outliers given these are descriptive counting numbers: the latitude, longitude, number of docks, and are unlikely to reflect an error in measurement. Even if there is a higher-than-average number of docks, there is no clear reason as to why it should be excluded, or how it will negatively affect the analysis 

summary(CleanedWeather$max_temperature_f) #examine the range of maximum temperatures
sd(CleanedWeather$max_temperature_f)
plot(CleanedWeather$date,CleanedWeather$max_temperature_f, xlab="Date", ylab="Maximum Temperature (F)")
summary(CleanedWeather$mean_temperature_f)
sd(CleanedWeather$mean_temperature_f)
plot(CleanedWeather$date,CleanedWeather$mean_temperature_f, xlab="Date", ylab="Mean Temperature (F)")
summary(CleanedWeather$min_temperature_f)
sd(CleanedWeather$min_temperature_f)
plot(CleanedWeather$date,CleanedWeather$min_temperature_f, xlab="Date", ylab="Minimum Temperature (F)")
summary(CleanedWeather$max_visibility_miles)
sd(CleanedWeather$max_visibility_miles)
plot(CleanedWeather$date,CleanedWeather$max_visibility_miles, xlab="Date", ylab="Maximum Visibility (miles)")
summary(CleanedWeather$mean_visibility_miles)
sd(CleanedWeather$mean_visibility_miles)
plot(CleanedWeather$date,CleanedWeather$mean_visibility_miles, xlab="Date", ylab="Mean Visibility (miles)")
summary(CleanedWeather$min_visibility_miles)
sd(CleanedWeather$min_visibility_miles)
plot(CleanedWeather$date,CleanedWeather$min_visibility_miles, , xlab="Date", ylab="Minimum Visibility (miles)")
summary(CleanedWeather$max_wind_Speed_mph)
sd(CleanedWeather$max_wind_Speed_mph)
plot(CleanedWeather$date,CleanedWeather$max_wind_Speed_mph, xlab="Date", ylab="Maximum Wind Speed (mph)")
summary(CleanedWeather$mean_wind_speed_mph)
sd(CleanedWeather$mean_wind_speed_mph)
plot(CleanedWeather$date,CleanedWeather$mean_wind_speed_mph, xlab="Date", ylab="Mean Wind Speed (mph)")
summary(CleanedWeather$max_gust_speed_mph)
sd(CleanedWeather$max_gust_speed_mph)
plot(CleanedWeather$date,CleanedWeather$max_gust_speed_mph, xlab="Date", ylab="Maximum Gust Speed (mph)")
summary(CleanedWeather$precipitation_inches)
sd(CleanedWeather$precipitation_inches)
plot(CleanedWeather$date,CleanedWeather$precipitation_inches, xlab="Date", ylab="Precipitation (inches)")
summary(CleanedWeather$cloud_cover)
sd(CleanedWeather$cloud_cover)
plot(CleanedWeather$date,CleanedWeather$cloud_cover, xlab="Date", ylab="Cloud Cover")

# f) Analyze categorical and numerical variables at the same time: get a sense of all the variables to be analyzed

basic_eda <- function(data)
{
  glimpse(data)
  print(status(data))
  freq(data) 
  print(profiling_num(data))
  plot_num(data)
  describe(data)
}

basic_eda(CleanedStation)
basic_eda(CleanedTrip)
basic_eda(CleanedWeather)

# 3. Record trip IDs for outliers, and then remove them. What are the rules for outliers? Outside of 95% of the data? Or a different metric?
CleanedTrip <- mutate(CleanedTrip, TripDuration = (CleanedTrip$end_date-CleanedTrip$start_date))
# figure out which trips lasted longer than 1 month
TripDurationOutlierIndices <- which(CleanedTrip$TripDuration>as.difftime(43200,units="mins"))
TripDurationOutliers <- CleanedTrip[TripDurationOutlierIndices,]
# 1440 minutes constitutes a full day of bike renting - Bay Area bike rental lengths usually go up to full-day or 24 hours (though there are some that can be rented on a weekly, or even monthly basis) - https://www.lyft.com/bikes/bay-wheels/sf-bike-rental
CleanedTrip <- CleanedTrip[-TripDurationOutlierIndices,]
Reduced = as.numeric(readline(prompt = "Please enter a 1 if you want to analyze 97.5% of the possible data, or enter 0 to analyze all possible data. Both exclude outliers"))
if (Reduced == 1){
  # determine which trip durations fall outside of the upper bounds of the 95% confidence interval
  TripDurationExtremeIndices <- which(CleanedTrip$TripDuration>=quantile(CleanedTrip$TripDuration,probs=0.975)) #this will include the outliers as well
  # Place the outliers in a new data frame
  TripDurationExtremeValues <- CleanedTrip[setdiff(TripDurationExtremeIndices,TripDurationOutlierIndices),]
  CleanedTrip <- CleanedTrip[-TripDurationExtremeIndices,] #remove the outlier indices from the main cleaned dataframe
}

# 4. Record and remove "cancelled trips" (i.e., any trip <3 minutes)

# 5. Determine weekday rush hours - What consists of rush hour? Provide a definition. Histograms by hour are a good idea to test, or 15 minutes to be more precise. Lubridate will be useful for determining time intervals

# 6. Determine 10 most frequent starting and ending stations during rush hours

# 7. Determine 10 most frequent starting and ending stations during the weekends

# 8. Determine the average utilization of bikes for each month (total time bikes were used/total time in the month)

# 9. Create a new dataset combining trip data with weather data. Weather data is available for each city and date. The cor function from the Corrplot package will be useful for creating a correlation matrix. Deal with the fact that it is difficult to correlate categorical and non-categorical variables. Some trips start and end in different cities, will need to explain how to correlate weather data with trip data if the trip is crossing cities - may have implications for how the two datasets are joined
