#Weather Analysis

#' Create a new dataset combining trip data with weather data. Weather data is 
#' available for each city and date. The cor function from the Corrplot package 
#' will be useful for creating a correlation matrix. Deal with the fact that it 
#' is difficult to correlate categorical and non-categorical variables. Some 
#' trips start and end in different cities, will need to explain how to correlate 
#' weather data with trip data if the trip is crossing cities - may have 
#' implications for how the two datasets are joined

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

# Copy over the data cleaning steps of the EDA, leaving out the steps that analyzed and plotted the data
RawWeather <- read.csv("weather.csv")
CleanedWeather <- RawWeather
CleanedWeather$date <- strptime(CleanedWeather$date, format = "%m/%d/%Y", tz="EST")
CleanedWeather$precipitation_inches[CleanedWeather$precipitation_inches=="T"]=0.001
CleanedWeather$precipitation_inches <- as.numeric(CleanedWeather$precipitation_inches)
CleanedWeather$events[CleanedWeather$events==""]="Not Recorded"
CleanedWeather$events[CleanedWeather$events=="rain"]="Rain"
CleanedWeather$events <- factor(CleanedWeather$events)
CleanedWeather$city <- factor(CleanedWeather$city)
CleanedWeather$zip_code <- factor(CleanedWeather$zip_code)
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
CleanedTrip$start_date <- strptime(RawTrip$start_date, format = "%m/%d/%Y %H:%M", tz="EST")
CleanedTrip$end_date <- strptime(RawTrip$end_date, format = "%m/%d/%Y %H:%M", tz="EST")
CleanedTrip <- mutate(CleanedTrip, TripDuration = (CleanedTrip$end_date-CleanedTrip$start_date))

StationName <- unique(CleanedTrip$start_station_name)
plot(CleanedStation$long,CleanedStation$lat, xlab = "Longitude", ylab = "Latitude", main = "Station Coordinates")

# figure out which trips lasted longer than 1 day
TripDurationOutlierIndices <- which(CleanedTrip$TripDuration>as.difftime(43200,units="mins"))
TripDurationOutliers <- CleanedTrip[TripDurationOutlierIndices,]
# 1440 minutes constitutes a full day of bike renting - Bay Area bike rental lengths usually go up to full-day or 24 hours (though there are some that can be rented on a weekly, or even monthly basis) - https://www.lyft.com/bikes/bay-wheels/sf-bike-rental

# determine which trip durations fall outside of the upper bounds of the 95% confidence interval
TripDurationExtremeIndices <- which(CleanedTrip$TripDuration>=quantile(CleanedTrip$TripDuration,probs=0.975)) #this will include the outliers as well
# Place the outliers in a new data frame
TripDurationExtremeValues <- CleanedTrip[setdiff(TripDurationExtremeIndices,TripDurationOutlierIndices),]

CleanedTrip <- CleanedTrip[-TripDurationExtremeIndices,] #remove the outlier indices from the main cleaned dataframe

# Record and remove "cancelled trips" (i.e., any trip <3 minutes)
CancelledTripIndices <- which(CleanedTrip$TripDuration<as.difftime(3,units="mins"))
CancelledTripValues <- CleanedTrip[CancelledTripIndices,]
CleanedTrip <- CleanedTrip[-CancelledTripIndices,]

# Need to make the dates applicable in teh trip dataset so that they work with the weather dataset
CleanedTrip$start_date <- floor_date(CleanedTrip$start_date, "day")

CityZipData <- left_join(x=CleanedStation, y=CleanedTrip, by=c("id"="start_station_id"))
CityZipData <- CityZipData[,c(2,3,4,6,10,12,13,14,17,18)]
CityZipData <- left_join(x=CityZipData, y=CleanedWeather, by=c("city"="city", "start_date"="date"))

# focus on the start date as the weather during the initiation of the trip would impact whether someone chooses to use a bike or use a different mode of transportation

CorrelationWeatherAndTripData1 <- CityZipData[,c(4,10:22,24)]
test1 <- model.matrix(~0+., data=CorrelationWeatherAndTripData1)
cor(x=test1[,c(7:27)],y=test1[,c(1:6)],use="pairwise.complete.obs") %>%
    ggcorrplot(show.diag=NULL, type="full", lab=TRUE, lab_size=2,
               title = "Weather and Trip Variable Correlation",
               colors = c("blue", "white", "red"))

cor(test1,use="pairwise.complete.obs") %>%
ggcorrplot(show.diag=NULL, type="full", lab=TRUE, lab_size=2,
               title = "Weather and Trip Variable Correlation",
               colors = c("blue", "white", "red"))


CorrelationWeatherAndTripData <- CityZipData[,c(1,10:22)]
test <- model.matrix(~0+., data=CorrelationWeatherAndTripData)
cor(x=test[,c(1:10)],y=test[,c(72:85)],use="pairwise.complete.obs") %>%
  corrplot(show.diag=NULL, method = "color", type="upper", mar=c(0,0,2,0),
           tl.col = "black",  cl.ratio = 0.5, cl.length=5, tl.offset=0,title = "Weather and Trip Variable Correlation")

cor(x=test[,c(11:20)],y=test[,c(72:85)],use="pairwise.complete.obs") %>%
  corrplot(show.diag=NULL, method = "color", type="upper", mar=c(0,0,2,0),
           tl.col = "black",  cl.ratio = 0.5, cl.length=5, tl.offset=0,title = "Weather and Trip Variable Correlation")

cor(x=test[,c(21:30)],y=test[,c(72:85)],use="pairwise.complete.obs") %>%
  corrplot(show.diag=NULL, method = "color", type="upper", mar=c(0,0,2,0),
           tl.col = "black",  cl.ratio = 0.5, cl.length=5, tl.offset=0,title = "Weather and Trip Variable Correlation")

cor(x=test[,c(31:40)],y=test[,c(72:85)],use="pairwise.complete.obs") %>%
  corrplot(show.diag=NULL, method = "color", type="upper", mar=c(0,0,2,0),
           tl.col = "black",  cl.ratio = 0.5, cl.length=5, tl.offset=0,title = "Weather and Trip Variable Correlation")

cor(x=test[,c(41:50)],y=test[,c(72:85)],use="pairwise.complete.obs") %>%
  corrplot(show.diag=NULL, method = "color", type="upper", mar=c(0,0,2,0),
           tl.col = "black",  cl.ratio = 0.5, cl.length=5, tl.offset=0,title = "Weather and Trip Variable Correlation")

cor(x=test[,c(51:60)],y=test[,c(72:85)],use="pairwise.complete.obs") %>%
  corrplot(show.diag=NULL, method = "color", type="upper", mar=c(0,0,2,0),
           tl.col = "black",  cl.ratio = 0.5, cl.length=5, tl.offset=0,title = "Weather and Trip Variable Correlation")

cor(x=test[,c(61:65)],y=test[,c(72:85)],use="pairwise.complete.obs") %>%
  corrplot(show.diag=NULL, method = "color", type="upper", mar=c(0,0,2,0),
           tl.col = "black",  cl.ratio = 0.5, cl.length=5, tl.offset=0,title = "Weather and Trip Variable Correlation")

cor(x=test[,c(66:71)],y=test[,c(72:85)],use="pairwise.complete.obs") %>%
  corrplot(show.diag=NULL, method = "color", type="upper", mar=c(0,0,2,0),
           tl.col = "black",  cl.ratio = 0.5, cl.length=5, tl.offset=0,title = "Weather and Trip Variable Correlation")


# Linear model

TripDurationLinearModel <- lm(as.numeric(TripDuration) ~ max_temperature_f+mean_temperature_f+min_temperature_f+max_visibility_miles+mean_visibility_miles+min_visibility_miles+max_wind_Speed_mph+mean_wind_speed_mph+max_gust_speed_mph+precipitation_inches+cloud_cover+events, data=CityZipData)
summary(TripDurationLinearModel)


NewWeatherTripData <- CityZipData[,c(4,5,11:22)]





