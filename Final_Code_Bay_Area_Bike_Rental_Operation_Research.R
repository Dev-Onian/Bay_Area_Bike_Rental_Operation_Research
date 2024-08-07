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

CancelledTripIndices <- which(CleanedTrip$TripDuration<as.difftime(3,units="mins")) #find which trips lasted less than 3 minutes
CancelledTripValues <- CleanedTrip[CancelledTripIndices,]
CleanedTrip <- CleanedTrip[-CancelledTripIndices,]

boxplot(CleanedTrip$TripDuration, #plot the final distribution of trips
        ylab = "Duration (mins)"
)
title("Cleaned Bike Trip Durations")

# 5. Determine weekday rush hours - What consists of rush hour? Provide a definition. Histograms by hour are a good idea to test, or 15 minutes to be more precise. Lubridate will be useful for determining time intervals

# add the durations for each trip to the CleanedTrip data frame
CleanedTrip <- mutate(CleanedTrip, TripInterval = interval(start_date,end_date))

# create a list of times of 15 minute intervals over the course of a full day
ListOfTimes <- interval(strptime("00:00", format = "%H:%M",tz="EST"),strptime("00:00", format = "%H:%M",tz="EST") +(15*60))
InitialTime1 <- strptime("00:00", format = "%H:%M",tz="EST")
InitialTime <- strptime("00:00", format = "%H:%M",tz="EST")
for (i in 1:95){ #there are 96 intervals, including the initial interval that is built upon in 15 minute increments
  InitialTime <- InitialTime +(15*60)
  InitialInterval <- interval(InitialTime,InitialTime +(15*60))
  ListOfTimes[i+1,]<-InitialInterval
}

# Calculate the number of days between the Trip Time interval and the base list of times
testdif <- floor(as.numeric(difftime(InitialTime1,CleanedTrip$start_date, units="days"))) #using the floor cuts off any minutes that may errantly shift the number of days between the trip interval and teh reference interval
# shift the Trip intervals to the date that the base list of times is set, allowing for comparison of the times
testshift <- int_shift(CleanedTrip$TripInterval,duration(days=testdif+1))
attr(testshift,"tzone") <- "EST" #convert the times to EST for ease of comparison with the original data

CleanedTrip <- mutate(CleanedTrip, ShiftedTripInterval = testshift) #add these shifted intervals to the cleaned trip data

RushHours <- data.frame(Interval = as.character(ListOfTimes[1]), NumberOfTrips = sum(int_overlaps(ListOfTimes[1],testshift))) #determine how many trips fall into the first interval
for (i in 1:95){ #determine how many trips fall within the subsequent 95 intervals
  Interval = as.character(ListOfTimes[i+1])
  NumberOfTrips = sum(int_overlaps(ListOfTimes[i+1],testshift))
  RushHours[i+1,] = list(Interval,NumberOfTrips)
}

RushHours <- cbind.data.frame(RushHours,POSIXInterval = ListOfTimes) #add the original reference list of times to the rush hours interval

RushHourTimes <-RushHours[which(RushHours$NumberOfTrips>=quantile(RushHours$NumberOfTrips,probs=0.8)),]
RushHourCutoff <- quantile(RushHours$NumberOfTrips,probs=0.8)

RushHours$Interval <- str_remove_all(RushHours$Interval,"\\d{4}[-]\\d{2}[-]\\d{2}") #remove the dates from the rush hour intervals, as these will serve as the x axis values for the graph
RushHours$Interval <- str_remove_all(RushHours$Interval,"[:]\\d{2}[ ]")
RushHours$Interval[1] <- "00:00EST--00:15EST" #account for the fact that the first and last intervals won't be formatted properly
RushHours$Interval[96] <- "23:45EST--00:00EST"

cols <- c("white", "red")[(RushHours$NumberOfTrips > RushHourCutoff) + 1]  
par(mar=c(6,4,4,1))
barplot(RushHours$NumberOfTrips, col = cols, names.arg=unlist(RushHours$Interval),las=2,
        ylab = "Number of Trips",
        main = "Bike Usage Stratified By Time",
        cex.names=0.5)

# 6. Determine 10 most frequent starting and ending stations during rush hours
WeekdayTrips <- CleanedTrip[-which(wday(CleanedTrip$start_date)==1 | wday(CleanedTrip$start_date)==7),]

RushHourTripIndices <- int_overlaps(RushHourTimes$POSIXInterval[1],WeekdayTrips$ShiftedTripInterval)
for (i in 1:nrow(RushHourTimes)){
  RushHourTripIndices <- append(RushHourTripIndices,int_overlaps(RushHourTimes$POSIXInterval[i],WeekdayTrips$ShiftedTripInterval))
} #Get an index of every trip that occurred during rush hour

RushHourTrips <- WeekdayTrips[RushHourTripIndices,]
(RushHourTrips$start_station_name)
TopRushHourStartingStations <- count(RushHourTrips,start_station_name,sort = TRUE)
TopRushHourEndingStations <- count(RushHourTrips,end_station_name,sort = TRUE)

# 7. Determine 10 most frequent starting and ending stations during the weekends

WeekendTrips <- CleanedTrip[which(wday(CleanedTrip$start_date)==1 | wday(CleanedTrip$start_date)==7),]
WeekendTrips <- mutate(WeekendTrips,Day = wday(WeekendTrips$start_date))
TopWeekendTripsStartingStations <- count(WeekendTrips,start_station_name,sort = TRUE)
TopWeekendTripsEndingStations <- count(WeekendTrips,end_station_name,sort = TRUE)

# 8. Determine the average utilization of bikes for each month (total time bikes were used/total time in the month)

CleanedTrip <- mutate(CleanedTrip, Month = month(CleanedTrip$start_date)) #which month does the trip take place in?

# calculate the length of time a bike was used according to the bike ID
BikeIDMonthUseBase <- aggregate(TripDuration~bike_id+Month,CleanedTrip,FUN=sum)

BikeIDMonthUseBase
ggplot(BikeIDMonthUseBase, mapping = aes(x=Month, y=as.numeric(TripDuration))) + geom_point()

BikeIDMonthUse <- pivot_wider(BikeIDMonthUseBase,names_from = Month, values_from = TripDuration)
# 2014 was not a leap year, hence February has 28 days
MonthDurations <- as.duration(days_in_month(1:12)*86400) #how many seconds are in each month?

BikeIDMonthUse[,2] <- BikeIDMonthUse[,2]/MonthDurations[1]
BikeIDMonthUse[,3] <- BikeIDMonthUse[,3]/MonthDurations[2]
BikeIDMonthUse[,4] <- BikeIDMonthUse[,4]/MonthDurations[3]
BikeIDMonthUse[,5] <- BikeIDMonthUse[,5]/MonthDurations[4]
BikeIDMonthUse[,6] <- BikeIDMonthUse[,6]/MonthDurations[5]
BikeIDMonthUse[,7] <- BikeIDMonthUse[,7]/MonthDurations[6]
BikeIDMonthUse[,8] <- BikeIDMonthUse[,8]/MonthDurations[7]
BikeIDMonthUse[,9] <- BikeIDMonthUse[,9]/MonthDurations[8]
BikeIDMonthUse[,10] <- BikeIDMonthUse[,10]/MonthDurations[9]
BikeIDMonthUse[,11] <- BikeIDMonthUse[,11]/MonthDurations[10]
BikeIDMonthUse[,12] <- BikeIDMonthUse[,12]/MonthDurations[11]
BikeIDMonthUse[,13] <- BikeIDMonthUse[,13]/MonthDurations[12]

TopJanuaryBikes <- arrange(BikeIDMonthUse[,c(1,2)], desc(BikeIDMonthUse[,2]))
TopFebruaryBikes <- arrange(BikeIDMonthUse[,c(1,3)], desc(BikeIDMonthUse[,3]))
TopMarchBikes <- arrange(BikeIDMonthUse[,c(1,4)], desc(BikeIDMonthUse[,4]))
TopAprilBikes <- arrange(BikeIDMonthUse[,c(1,5)], desc(BikeIDMonthUse[,5]))
TopMayBikes <- arrange(BikeIDMonthUse[,c(1,6)], desc(BikeIDMonthUse[,6]))
TopJuneBikes <- arrange(BikeIDMonthUse[,c(1,7)], desc(BikeIDMonthUse[,7]))
TopJulyBikes <- arrange(BikeIDMonthUse[,c(1,8)], desc(BikeIDMonthUse[,8]))
TopAugustBikes <- arrange(BikeIDMonthUse[,c(1,9)], desc(BikeIDMonthUse[,9]))
TopSeptemberBikes <- arrange(BikeIDMonthUse[,c(1,10)], desc(BikeIDMonthUse[,10]))
TopOctoberBikes <- arrange(BikeIDMonthUse[,c(1,11)], desc(BikeIDMonthUse[,11]))
TopNovemebrBikes <- arrange(BikeIDMonthUse[,c(1,12)], desc(BikeIDMonthUse[,12]))
TopDecemberBikes <- arrange(BikeIDMonthUse[,c(1,13)], desc(BikeIDMonthUse[,13]))

TopBikes <- cbind(TopJanuaryBikes$bike_id,
                  TopFebruaryBikes$bike_id,
                  TopMarchBikes$bike_id,
                  TopAprilBikes$bike_id,
                  TopMayBikes$bike_id,
                  TopJuneBikes$bike_id,
                  TopJulyBikes$bike_id,
                  TopAugustBikes$bike_id,
                  TopSeptemberBikes$bike_id,
                  TopOctoberBikes$bike_id,
                  TopNovemebrBikes$bike_id,
                  TopDecemberBikes$bike_id)

AverageBikeUsage <- rbind(cbind(mean(as.numeric(unlist(TopJanuaryBikes[,2])), na.rm=T),
                                mean(as.numeric(unlist(TopFebruaryBikes[,2])), na.rm=T),
                                mean(as.numeric(unlist(TopMarchBikes[,2])), na.rm=T),
                                mean(as.numeric(unlist(TopAprilBikes[,2])), na.rm=T),
                                mean(as.numeric(unlist(TopMayBikes[,2])), na.rm=T),
                                mean(as.numeric(unlist(TopJuneBikes[,2])), na.rm=T),
                                mean(as.numeric(unlist(TopJulyBikes[,2])), na.rm=T),
                                mean(as.numeric(unlist(TopAugustBikes[,2])), na.rm=T),
                                mean(as.numeric(unlist(TopSeptemberBikes[,2])), na.rm=T),
                                mean(as.numeric(unlist(TopOctoberBikes[,2])), na.rm=T),
                                mean(as.numeric(unlist(TopNovemebrBikes[,2])), na.rm=T),
                                mean(as.numeric(unlist(TopDecemberBikes[,2])), na.rm=T)),c(1:12))
plot(x = AverageBikeUsage[2,], y= AverageBikeUsage[1,]*100, xlab="Month", ylab="Average Monthly Bike Utilization (%)", main="Average Bike Usage per Month")

TransposeTest <- as.data.frame(t(BikeIDMonthUse[,-1]))
colnames(TransposeTest) <- BikeIDMonthUse$bike_id

# Calculate bike usage independent of ID

OverallBikeUsage <- CleanedTrip[,c(3,6,15)]
OverallBikeUsage <- mutate(OverallBikeUsage, TripInterval = iv(OverallBikeUsage$start_date,OverallBikeUsage$end_date))
OverallBikeUsage <- OverallBikeUsage %>%
  group_by(Month) %>%
  reframe(TripInterval = iv_groups(TripInterval))

OverallBikeUsage <- mutate(OverallBikeUsage, TripDuration = (iv_end(TripInterval)-iv_start(TripInterval)))
OverallBikeUsage <- aggregate(TripDuration~Month,OverallBikeUsage,FUN=sum)

#MonthDurations <- as.numeric(MonthDurations)
OverallBikeUsage <- c(OverallBikeUsage[1,2]/MonthDurations[1],
                      OverallBikeUsage[2,2]/MonthDurations[2],
                      OverallBikeUsage[3,2]/MonthDurations[3],
                      OverallBikeUsage[4,2]/MonthDurations[4],
                      OverallBikeUsage[5,2]/MonthDurations[5],
                      OverallBikeUsage[6,2]/MonthDurations[6],
                      OverallBikeUsage[7,2]/MonthDurations[7],
                      OverallBikeUsage[8,2]/MonthDurations[8],
                      OverallBikeUsage[9,2]/MonthDurations[9],
                      OverallBikeUsage[10,2]/MonthDurations[10],
                      OverallBikeUsage[11,2]/MonthDurations[11],
                      OverallBikeUsage[12,2]/MonthDurations[12])
OverallBikeUsage <- cbind(OverallBikeUsage,c(1:12))
plot(x = OverallBikeUsage[,2], y= OverallBikeUsage[,1]*100, xlab="Month", ylab="Monthly Bike Utilization (%)", main="Bike Usage per Month")

# 9. Create a new dataset combining trip data with weather data. Weather data is available for each city and date. The cor function from the Corrplot package will be useful for creating a correlation matrix. Deal with the fact that it is difficult to correlate categorical and non-categorical variables. Some trips start and end in different cities, will need to explain how to correlate weather data with trip data if the trip is crossing cities - may have implications for how the two datasets are joined



