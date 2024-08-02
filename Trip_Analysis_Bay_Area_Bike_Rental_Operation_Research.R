# Trip_Analysis
library(tidyverse)
library(funModeling)
library(Hmisc)
library(dplyr)

# Copy over the data cleaning steps of the EDA, leaving out the steps that analyzed and plotted the data
RawWeather <- read.csv("weather.csv")
CleanedWeather <- RawWeather
CleanedWeather$date <- strptime(CleanedWeather$date, format = "%m/%d/%Y")
CleanedWeather$precipitation_inches[CleanedWeather$precipitation_inches=="T"]=0.001
CleanedWeather$precipitation_inches <- as.numeric(CleanedWeather$precipitation_inches)
CleanedWeather$events[CleanedWeather$events==""]="Not Recorded"
CleanedWeather$events[CleanedWeather$events=="rain"]="Rain"
CleanedWeather$events <- factor(CleanedWeather$events)
CleanedWeather$city <- factor(CleanedWeather$city)
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

RawStation <- read.csv("station.csv")
CleanedStation <- RawStation
CleanedStation$id <- factor(CleanedStation$id)
CleanedStation$name <- factor(CleanedStation$name)
CleanedStation$city <- factor(CleanedStation$city)
CleanedStation$installation_date <- strptime(CleanedStation$installation_date, format = "%m/%d/%Y")

RawTrip <- read.csv("trip.csv")
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
CleanedTrip <- mutate(CleanedTrip, TripDuration = (CleanedTrip$end_date-CleanedTrip$start_date))
# The longest trip is ~200 days long

# 3. Record trip IDs for outliers, and then remove them. What are the rules for outliers? Outside of 95% of the data? Or a different metric?

boxplot(CleanedTrip$TripDuration,
        ylab = "Duration"
)
# index 229947 makes it impossible to see the rest of the data. It is 287839 minutes, while the next longest duration is only 12007 minutes
boxplot(CleanedTrip$TripDuration[-229947],
        ylab = "Duration"
)
# the 2nd longest trip is 8 days, which is technically possible given some bay area bikes can be rented out for a month - https://www.lyft.com/bikes/bay-wheels/sf-bike-rental
# Though the long durations of the trips compared to the vast majority may make them extreme, possible values that may be beyond the purview of the analysis

TripDurationOutlierIndices <- which(CleanedTrip$TripDuration>as.difftime(1440,units="mins"))
TripDurationOutliers <- CleanedTrip[TripDurationOutlierIndices,]
# 1440 minutes constitutes a full day of bike renting - Bay Area bike rental lengths usually go up to full-day or 24 hours (though there are some that can be rented on a weekly, or even monthly basis) - https://www.lyft.com/bikes/bay-wheels/sf-bike-rental
boxplot(CleanedTrip$TripDuration[-TripDurationOutlierIndices],
        ylab = "Duration"
)

TripDurationExtremeIndices <- which(CleanedTrip$TripDuration>=quantile(CleanedTrip$TripDuration,probs=0.975)) #this will include the outliers as well
TripDurationExtremeValues <- CleanedTrip[setdiff(TripDurationExtremeIndices,TripDurationOutlierIndices),]
boxplot(CleanedTrip$TripDuration[-TripDurationExtremeIndices],
        ylab = "Duration"
)

CleanedTrip <- CleanedTrip[-TripDurationExtremeIndices,] #remove the outlier indices from the main cleaned dataframe

# 4. Record and remove "cancelled trips" (i.e., any trip <3 minutes)

CancelledTripIndices <- which(CleanedTrip$TripDuration<as.difftime(3,units="mins"))
CancelledTripValues <- CleanedTrip[CancelledTripIndices,]
CleanedTrip <- CleanedTrip[-CancelledTripIndices,]

# 5. Determine weekday rush hours - What consists of rush hour? Provide a definition. Histograms by hour are a good idea to test, or 15 minutes to be more precise. Lubridate will be useful for determining time intervals



# 6. Determine 10 most frequent starting and ending stations during rush hours



# 7. Determine 10 most frequent starting and ending stations during the weekends



# 8. Determine the average utilization of bikes for each month (total time bikes were used/total time in the month)


