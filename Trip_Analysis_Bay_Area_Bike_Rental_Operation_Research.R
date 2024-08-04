# Trip_Analysis
library(tidyverse)
library(funModeling)
library(Hmisc)
library(dplyr)
library(lubridate)
library(stringr)

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
CleanedTrip$start_date <- strptime(RawTrip$start_date, format = "%m/%d/%Y %H:%M", tz="EST")
CleanedTrip$end_date <- strptime(RawTrip$end_date, format = "%m/%d/%Y %H:%M", tz="EST")
CleanedTrip <- mutate(CleanedTrip, TripDuration = (CleanedTrip$end_date-CleanedTrip$start_date))

# 3. Record trip IDs for outliers, and then remove them. What are the rules for outliers? Outside of 95% of the data? Or a different metric?

# get a sense of what the trip duration data looks like
boxplot(CleanedTrip$TripDuration,
        ylab = "Duration (mins)"
)
# index 229947 makes it impossible to see the rest of the data. It is 287839 minutes (~200 days long), while the next longest duration is only 12007 minutes (~8 days)
boxplot(CleanedTrip$TripDuration[-229947],
        ylab = "Duration (mins)"
)
# the 2nd longest trip is 8 days, which is technically possible given some bay area bikes can be rented out for up to a month - https://www.lyft.com/bikes/bay-wheels/sf-bike-rental
# Though the long durations of the trips compared to the vast majority may make them possible values, they may still be extreme and beyond the purview of the analysis

# figure out which trips lasted longer than 1 day
TripDurationOutlierIndices <- which(CleanedTrip$TripDuration>as.difftime(1440,units="mins"))
TripDurationOutliers <- CleanedTrip[TripDurationOutlierIndices,]
# 1440 minutes constitutes a full day of bike renting - Bay Area bike rental lengths usually go up to full-day or 24 hours (though there are some that can be rented on a weekly, or even monthly basis) - https://www.lyft.com/bikes/bay-wheels/sf-bike-rental
boxplot(CleanedTrip$TripDuration[-TripDurationOutlierIndices],
        ylab = "Duration (mins)"
)

# determine which trip durations fall outside of the upper bounds of the 95% confidence interval
TripDurationExtremeIndices <- which(CleanedTrip$TripDuration>=quantile(CleanedTrip$TripDuration,probs=0.975)) #this will include the outliers as well
# Place the outliers in a new data frame
TripDurationExtremeValues <- CleanedTrip[setdiff(TripDurationExtremeIndices,TripDurationOutlierIndices),]
boxplot(CleanedTrip$TripDuration[-TripDurationExtremeIndices],
        ylab = "Duration (mins)"
)

CleanedTrip <- CleanedTrip[-TripDurationExtremeIndices,] #remove the outlier indices from the main cleaned dataframe

# 4. Record and remove "cancelled trips" (i.e., any trip <3 minutes)

CancelledTripIndices <- which(CleanedTrip$TripDuration<as.difftime(3,units="mins"))
CancelledTripValues <- CleanedTrip[CancelledTripIndices,]
CleanedTrip <- CleanedTrip[-CancelledTripIndices,]

# 5. Determine weekday rush hours - What consists of rush hour? Provide a definition. Histograms by hour are a good idea to test, or 15 minutes to be more precise. Lubridate will be useful for determining time intervals

# add the durations for each trip to the CleanedTrip data frame
CleanedTrip <- mutate(CleanedTrip, TripInterval = interval(start_date,end_date))

# create a list of times of 15 minute intervals over the course of a full day
ListOfTimes <- interval(strptime("00:00", format = "%H:%M",tz="EST"),strptime("00:00", format = "%H:%M",tz="EST") +(15*60))
InitialTime1 <- strptime("00:00", format = "%H:%M",tz="EST")
InitialTime <- strptime("00:00", format = "%H:%M",tz="EST")
for (i in 1:95){
  InitialTime <- InitialTime +(15*60)
  InitialInterval <- interval(InitialTime,InitialTime +(15*60))
  ListOfTimes[i+1,]<-InitialInterval
}

# Calculate the number of days between the Trip Time interval and the base list of times
testdif <- floor(as.numeric(difftime(InitialTime1,CleanedTrip$start_date, units="days")))
# shift the Trip intervals to the date that the base list of times is set, allowing for comparison of the times
testshift <- int_shift(CleanedTrip$TripInterval,duration(days=testdif+1))
attr(testshift,"tzone") <- "EST"

CleanedTrip <- mutate(CleanedTrip, ShiftedTripInterval = testshift)

RushHours <- data.frame(Interval = as.character(ListOfTimes[1]), NumberOfTrips = sum(int_overlaps(ListOfTimes[1],testshift)))
for (i in 1:95){
  Interval = as.character(ListOfTimes[i+1])
  NumberOfTrips = sum(int_overlaps(ListOfTimes[i+1],testshift))
  RushHours[i+1,] = list(Interval,NumberOfTrips)
}

RushHours <- cbind.data.frame(RushHours,POSIXInterval = ListOfTimes)

RushHourTimes <-RushHours[which(RushHours$NumberOfTrips>=quantile(RushHours$NumberOfTrips,probs=0.8)),]

RushHours$Interval <- str_remove_all(RushHours$Interval,"\\d{4}[-]\\d{2}[-]\\d{2}")
RushHours$Interval <- str_remove_all(RushHours$Interval,"[:]\\d{2}[ ]")
RushHours$Interval[1] <- "00:00EST--00:15EST"
RushHours$Interval[96] <- "23:45EST--00:00EST"

par(mar=c(6,4,4,1))
barplot(RushHours$NumberOfTrips, names.arg=unlist(RushHours$Interval),las=2,
        ylab = "Number of Trips",
        main = "Bike Usage Stratified By Time",
        cex.names=0.5)

# 6. Determine 10 most frequent starting and ending stations during rush hours

RushHourTripIndices <- int_overlaps(RushHourTimes$POSIXInterval[1],CleanedTrip$ShiftedTripInterval)
for (i in 1:nrow(RushHourTimes)){
  RushHourTripIndices <- RushHourTripIndices + int_overlaps(RushHourTimes$POSIXInterval[i],CleanedTrip$ShiftedTripInterval)
} #Get an index of every trip that occurred during rush hour

RushHourTrips <- CleanedTrip[RushHourTripIndices,]
(RushHourTrips$start_station_name)
TopRushHourStartingStations <- count(RushHourTrips,start_station_name,sort = TRUE)
TopRushHourEndingStations <- count(RushHourTrips,end_station_name,sort = TRUE)

# 7. Determine 10 most frequent starting and ending stations during the weekends

WeekendTrips <- CleanedTrip[which(wday(CleanedTrip$start_date)==1 | wday(CleanedTrip$start_date)==7),]
WeekendTrips <- mutate(WeekendTrips,Day = wday(WeekendTrips$start_date))
TopWeekendTripsStartingStations <- count(WeekendTrips,start_station_name,sort = TRUE)
TopWeekendTripsEndingStations <- count(WeekendTrips,end_station_name,sort = TRUE)

# 8. Determine the average utilization of bikes for each month (total time bikes were used/total time in the month)


