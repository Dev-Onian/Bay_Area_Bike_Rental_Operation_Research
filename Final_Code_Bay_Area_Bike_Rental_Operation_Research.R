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
library(plyr)

# THE FOLLOWING EDA WILL EXAMINE STATION DATA, TRIP DATA, AND WEATHER DATA
# b) Examine data, make sure there is enough for suitable analysis, make sure everything is the right form

#load in the data in the csv file into a separate variable before examining the data
RawStation <- read.csv("station.csv")
summary(RawStation)
sum(as.character(RawStation$id) == "") #if this equals 0 then there are no missing values that aren't coded as NA
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
summary(CleanedStation) # ensure that the variables in station are coded properly

CleanedTrip <- RawTrip
CleanedTrip$id <- factor(CleanedTrip$id)
CleanedTrip$start_station_name <- factor(CleanedTrip$start_station_name)
CleanedTrip$start_station_id <- factor(CleanedTrip$start_station_id)
CleanedTrip$end_station_name <- factor(CleanedTrip$end_station_name)
CleanedTrip$end_station_id <- factor(CleanedTrip$end_station_id)
CleanedTrip$bike_id <- factor(CleanedTrip$bike_id)
CleanedTrip$subscription_type <- factor(CleanedTrip$subscription_type)
CleanedTrip$zip_code <- factor(CleanedTrip$zip_code) #zip codes should be factors instead of numbers as they represent locations instead of numerical values
CleanedTrip$start_date <- strptime(RawTrip$start_date, format = "%m/%d/%Y %H:%M")
CleanedTrip$end_date <- strptime(RawTrip$end_date, format = "%m/%d/%Y %H:%M")
summary(CleanedTrip) # ensure that the variables in trip are coded properly

CleanedWeather <- RawWeather
CleanedWeather$date <- strptime(CleanedWeather$date, format = "%m/%d/%Y") #convert the dates from strings to POSIX values for later processing
CleanedWeather$precipitation_inches[CleanedWeather$precipitation_inches=="T"]=0.001 # precipitation makes more sense as a numeric variable, and so trace values must be coded with a plausible numeric value
CleanedWeather$precipitation_inches <- as.numeric(CleanedWeather$precipitation_inches)
CleanedWeather$events[CleanedWeather$events==""]="Not Recorded"
CleanedWeather$events[CleanedWeather$events=="rain"]="Rain"
CleanedWeather$events <- factor(CleanedWeather$events)
CleanedWeather$city <- factor(CleanedWeather$city)
summary(CleanedWeather) # ensure that the variables in weather are coded properly

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

CleanedWeather <- mutate(CleanedWeather, ImputedEvents = case_when( #code for the combinations of recorded precipitation and visibility variables
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
sd(CleanedWeather$max_temperature_f) #determine teh standard deviation of the maximum temperatures
plot(CleanedWeather$date,CleanedWeather$max_temperature_f, xlab="Date", ylab="Maximum Temperature (F)")
# repeat this analysis for the other weather variables
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
plot(CleanedWeather$date,CleanedWeather$cloud_cover, xlab="Date", ylab="Cloud Cover (Oktas)")

# Remove weather days that reflect days that have tropical storm level weather conditions
HurricaneWindIndices <- which(CleanedWeather$max_wind_Speed_mph>50) # Tropical storms have winds in excess of 50 mph - https://www.weather.gov/lix/htiwind#:~:text=Hurricane%20winds%2074%20to%2090,of%20mobile%20homes%20is%20likely.
HurricaneWinds <- CleanedWeather[HurricaneWindIndices,] #store the tropical storm indices
CleanedWeather <- CleanedWeather[-HurricaneWindIndices,]

HurricaneGustIndices <- which(CleanedWeather$max_gust_speed_mph>65) # Tropical storms are where gusts begin to exceed 65 mph - https://www.weather.gov/lix/htiwind#:~:text=Hurricane%20winds%2074%20to%2090,of%20mobile%20homes%20is%20likely.
HurricaneGusts <- CleanedWeather[HurricaneGustIndices,]
CleanedWeather <- CleanedWeather[-HurricaneGustIndices,]


# f) Analyze categorical and numerical variables at the same time: get a sense of all the variables to be analyzed

# examine and plot the frequencies, range of values, and data types for the 3 cleaned datasets
glimpse(CleanedStation)
print(status(CleanedStation))
freq(CleanedStation) 
print(profiling_num(CleanedStation))
plot_num(CleanedStation[,c(1:6)])
describe(CleanedStation[,c(1:6)])

glimpse(CleanedTrip)
print(status(CleanedTrip))
freq(CleanedTrip[,c(2:8,10)]) 
print(profiling_num(CleanedTrip))
describe(CleanedTrip[,c(1:2,4:5,7:11)])

glimpse(CleanedWeather)
print(status(CleanedWeather))
freq(CleanedWeather) 
print(profiling_num(CleanedWeather))
plot_num(CleanedWeather[,c(2:16)])
describe(CleanedWeather[,c(2:16)])

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
  freq(TripDurationExtremeValues$subscription_type) 
  CleanedTrip <- CleanedTrip[-TripDurationExtremeIndices,] #remove the outlier indices from the main cleaned dataframe
}

# 4. Record and remove "cancelled trips" (i.e., any trip <3 minutes)

CancelledTripIndices <- which(CleanedTrip$TripDuration<as.difftime(3,units="mins")) #find which trips lasted less than 3 minutes
CancelledTripValues <- CleanedTrip[CancelledTripIndices,] #store the cancelled trips in a separate array
CleanedTrip <- CleanedTrip[-CancelledTripIndices,] #remove cancelled trips from the main dataset

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
  Interval = as.character(ListOfTimes[i+1]) #establish the new interval
  NumberOfTrips = sum(int_overlaps(ListOfTimes[i+1],testshift)) #determine which times overlap with this reference interval
  RushHours[i+1,] = list(Interval,NumberOfTrips) #add the number of trips that overlapped with the interval into the next row of RushHours
}

RushHours <- cbind.data.frame(RushHours,POSIXInterval = ListOfTimes) #add the original reference list of times to the rush hours interval

RushHourTimes <-RushHours[which(RushHours$NumberOfTrips>=quantile(RushHours$NumberOfTrips,probs=0.8)),] #which times are in the 80th percentile or higher for number of trips
RushHourCutoff <- quantile(RushHours$NumberOfTrips,probs=0.8) #what is the 80th percentile for rush hours, what is the cutoff between non rush hours and rush hours?

RushHours$Interval <- str_remove_all(RushHours$Interval,"\\d{4}[-]\\d{2}[-]\\d{2}") #remove the dates from the rush hour intervals, as these will serve as the x axis values for the graph
RushHours$Interval <- str_remove_all(RushHours$Interval,"[:]\\d{2}[ ]") # remove the seconds for ease of ditting the intervals on teh x axis
RushHours$Interval[1] <- "00:00EST--00:15EST" #account for the fact that the first and last intervals won't be formatted properly
RushHours$Interval[96] <- "23:45EST--00:00EST"

cols <- c("white", "red")[(RushHours$NumberOfTrips > RushHourCutoff) + 1]   # mark the rush hours as red
par(mar=c(6,4,4,1))
barplot(RushHours$NumberOfTrips, col = cols, names.arg=unlist(RushHours$Interval),las=2, #plot the rush hours
        ylab = "Number of Trips",
        main = "Bike Usage Stratified By Time",
        cex.names=0.5)

# 6. Determine 10 most frequent starting and ending stations during rush hours
WeekdayTrips <- CleanedTrip[-which(wday(CleanedTrip$start_date)==1 | wday(CleanedTrip$start_date)==7),] #separate the trips that DON'T take place on Saturday or Sunday

RushHourTripIndices <- int_overlaps(RushHourTimes$POSIXInterval[1],WeekdayTrips$ShiftedTripInterval) #find the trips that take place during rush hours from the weekday subset
for (i in 1:nrow(RushHourTimes)){
  RushHourTripIndices <- append(RushHourTripIndices,int_overlaps(RushHourTimes$POSIXInterval[i],WeekdayTrips$ShiftedTripInterval))
} #Get an index of every trip that occurred during rush hour

RushHourTrips <- WeekdayTrips[RushHourTripIndices,] #create a subset of trips that take place during rush hours on weekdays
(RushHourTrips$start_station_name)
TopRushHourStartingStations <- dplyr::count(RushHourTrips,start_station_name,sort = TRUE) #sort the trips in order of most common starting station
TopRushHourEndingStations <- dplyr::count(RushHourTrips,end_station_name,sort = TRUE)

# 7. Determine 10 most frequent starting and ending stations during the weekends

WeekendTrips <- CleanedTrip[which(wday(CleanedTrip$start_date)==1 | wday(CleanedTrip$start_date)==7),]
WeekendTrips <- mutate(WeekendTrips,Day = wday(WeekendTrips$start_date))
TopWeekendTripsStartingStations <- dplyr::count(WeekendTrips,start_station_name,sort = TRUE)
TopWeekendTripsEndingStations <- dplyr::count(WeekendTrips,end_station_name,sort = TRUE)

# Determine what the breakdown of subscribers and customers is across weekdays and weekends
WeekdaySubscribers <- sum(WeekdayTrips$subscription_type=="Subscriber")
WeekdayCustomers <- sum(WeekdayTrips$subscription_type=="Customer")
WeekendSubscribers <- sum(WeekendTrips$subscription_type=="Subscriber")
WeekendCustomers <- sum(WeekendTrips$subscription_type=="Customer")

# 8. Determine the average utilization of bikes for each month (total time bikes were used/total time in the month)

CleanedTrip <- mutate(CleanedTrip, Month = month(CleanedTrip$start_date)) #which month does the trip take place in?

# calculate the length of time a bike was used according to the bike ID
BikeIDMonthUseBase <- aggregate(TripDuration~bike_id+Month,CleanedTrip,FUN=sum)

BikeIDMonthUseBaseNamed <- BikeIDMonthUseBase[]
BikeIDMonthUseBaseNamed$Month <- month(BikeIDMonthUseBaseNamed$Month, label=TRUE)
ggplot(BikeIDMonthUseBaseNamed, mapping = aes(x=Month, y=as.numeric(TripDuration))) + geom_point()

BikeIDMonthUse <- pivot_wider(BikeIDMonthUseBase,names_from = Month, values_from = TripDuration)
# 2014 was not a leap year, hence February has 28 days
MonthDurations <- as.duration(days_in_month(1:12)*86400) #how many seconds are in each month?

#' calculate the ratio between the collective bike usage over the course of a 
#' month and the length of that month. In essence, for how much of a month are
#' there bikes in use?
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

# calculate the most used bikes for each month, whichever have the longest cumulative trip durations
TopJanuaryBikes <- dplyr::arrange(BikeIDMonthUse[,c(1,2)], desc(BikeIDMonthUse[,2]))
TopFebruaryBikes <- dplyr::arrange(BikeIDMonthUse[,c(1,3)], desc(BikeIDMonthUse[,3]))
TopMarchBikes <- dplyr::arrange(BikeIDMonthUse[,c(1,4)], desc(BikeIDMonthUse[,4]))
TopAprilBikes <- dplyr::arrange(BikeIDMonthUse[,c(1,5)], desc(BikeIDMonthUse[,5]))
TopMayBikes <- dplyr::arrange(BikeIDMonthUse[,c(1,6)], desc(BikeIDMonthUse[,6]))
TopJuneBikes <- dplyr::arrange(BikeIDMonthUse[,c(1,7)], desc(BikeIDMonthUse[,7]))
TopJulyBikes <- dplyr::arrange(BikeIDMonthUse[,c(1,8)], desc(BikeIDMonthUse[,8]))
TopAugustBikes <- dplyr::arrange(BikeIDMonthUse[,c(1,9)], desc(BikeIDMonthUse[,9]))
TopSeptemberBikes <- dplyr::arrange(BikeIDMonthUse[,c(1,10)], desc(BikeIDMonthUse[,10]))
TopOctoberBikes <- dplyr::arrange(BikeIDMonthUse[,c(1,11)], desc(BikeIDMonthUse[,11]))
TopNovemebrBikes <- dplyr::arrange(BikeIDMonthUse[,c(1,12)], desc(BikeIDMonthUse[,12]))
TopDecemberBikes <- dplyr::arrange(BikeIDMonthUse[,c(1,13)], desc(BikeIDMonthUse[,13]))

# bind the rows of top monthly bikes together to get an ordered list of all bikes for all months
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

# calculate the average cumulative trip duration for each bike per month by unlisting
# and averaging out each ordered list of bikes in each month
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

if (Reduced==1){ #This only works for the reduced dataset, otherwise the trips last so long that they spill over into other months
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
}
# 9. Create a new dataset combining trip data with weather data. Weather data is available for each city and date. The cor function from the Corrplot package will be useful for creating a correlation matrix. Deal with the fact that it is difficult to correlate categorical and non-categorical variables. Some trips start and end in different cities, will need to explain how to correlate weather data with trip data if the trip is crossing cities - may have implications for how the two datasets are joined

#' Focus on the starting dates for trips, rather than teh ending dates. Given the
#' purpose of this analysis, it is more reasonable that the initial weather
#' conditions will have a greater impact on whether a trip begins or not, rather
#' than the end weather. If the weather is poor, it may discourage someone from
#' using a bike. If the weather is favourable, it may encourage them to bike
#' instead of drive or take public transit. Now it is true that weather may impact
#' when someone returns a bike, but to account for that the analysis will also
#' look at how the initial weather conditions impact trip duration

# Need to make the dates applicable in teh trip dataset so that they work with the weather dataset
CleanedTrip$start_date <- floor_date(CleanedTrip$start_date, "day") #make the start dates applicable for comparison with the dates shown in the weather data
CityZipData <- left_join(x=CleanedStation, y=CleanedTrip, by=c("id"="start_station_id"))
CityZipData <- CityZipData[,c(2,3,4,6,10,12,13,14,16,17,18)]
CityZipData <- left_join(x=CityZipData, y=CleanedWeather, by=c("city"="city", "start_date"="date"))

# focus on the start date as the weather during the initiation of the trip would impact whether someone chooses to use a bike or use a different mode of transportation

# It is plausible that customers and subscribers behave differently, and have different purposes for using bikes (i.e. recreation vs work)

# correlation for all riders
NewWeatherTripData <- ddply(CityZipData[,c(4,5,11:22)],.(city,start_date),nrow)
names(NewWeatherTripData)[names(NewWeatherTripData) == 'V1'] <- "Number of Trips"
NewWeatherTripData <- left_join(x=NewWeatherTripData, y=CityZipData[,c(4,5,11:23)], by=c("city"="city", "start_date"="start_date"))
CorrelationWeatherAndTripData1 <- NewWeatherTripData[,c(3:16)]
test1 <- model.matrix(~0+., data=CorrelationWeatherAndTripData1)
cor(x=test1[,c(3:17)],y=test1[,c(1,2)],use="pairwise.complete.obs") %>%
  ggcorrplot(show.diag=NULL, type="full", lab=TRUE, lab_size=2,
             title = "Weather and Trip Variable Correlation for all Users",
             colors = c("blue", "white", "red"))

# correlation for customers
CityZipDataCustomer <- CityZipData[CityZipData$subscription_type=="Customer",]
NewWeatherTripData <- ddply(CityZipDataCustomer[,c(4,5,11:22)],.(city,start_date),nrow)
names(NewWeatherTripData)[names(NewWeatherTripData) == 'V1'] <- "Number of Trips"
NewWeatherTripData <- left_join(x=NewWeatherTripData, y=CityZipDataCustomer[,c(4,5,11:23)], by=c("city"="city", "start_date"="start_date"))
CorrelationWeatherAndTripData1 <- NewWeatherTripData[,c(3:16)]
test1 <- model.matrix(~0+., data=CorrelationWeatherAndTripData1)
cor(x=test1[,c(3:17)],y=test1[,c(1,2)],use="pairwise.complete.obs") %>%
  ggcorrplot(show.diag=NULL, type="full", lab=TRUE, lab_size=2,
             title = "Weather and Trip Variable Correlation for Customers",
             colors = c("blue", "white", "red"))

# correlation for subscribers
CityZipDataSubscriber <- CityZipData[CityZipData$subscription_type=="Subscriber",]
NewWeatherTripData <- ddply(CityZipDataSubscriber[,c(4,5,11:22)],.(city,start_date),nrow)
names(NewWeatherTripData)[names(NewWeatherTripData) == 'V1'] <- "Number of Trips"
NewWeatherTripData <- left_join(x=NewWeatherTripData, y=CityZipDataSubscriber[,c(4,5,11:23)], by=c("city"="city", "start_date"="start_date"))
CorrelationWeatherAndTripData1 <- NewWeatherTripData[,c(3:16)]
test1 <- model.matrix(~0+., data=CorrelationWeatherAndTripData1)
cor(x=test1[,c(3:17)],y=test1[,c(1,2)],use="pairwise.complete.obs") %>%
  ggcorrplot(show.diag=NULL, type="full", lab=TRUE, lab_size=2,
             title = "Weather and Trip Variable Correlation for Subscribers",
             colors = c("blue", "white", "red"))


# correlation for city and weather
CorrelationWeatherAndTripData1 <- CityZipData[,c(4,11:22)]
test1 <- model.matrix(~0+., data=CorrelationWeatherAndTripData1)
cor(x=test1[,c(7:17)],y=test1[,c(1:5)],use="pairwise.complete.obs") %>%
  ggcorrplot(show.diag=NULL, type="full", lab=TRUE, lab_size=2,
             title = "Weather and Starting City Correlation",
             colors = c("blue", "white", "red"))


# perform individual correlations for each station and weather
# the results are essentially no correlation with any weather

CorrelationWeatherAndTripData <- CityZipData[,c(1,11:22)]
test <- model.matrix(~0+., data=CorrelationWeatherAndTripData)
cor(x=test[,c(1:10)],y=test[,c(72:82)],use="pairwise.complete.obs") %>%
  corrplot(show.diag=NULL, method = "color", type="upper", mar=c(0,0,2,0),
           tl.col = "black",  cl.ratio = 0.5, cl.length=5, tl.offset=0,title = "Weather and Trip Variable Correlation")

cor(x=test[,c(11:20)],y=test[,c(72:82)],use="pairwise.complete.obs") %>%
  corrplot(show.diag=NULL, method = "color", type="upper", mar=c(0,0,2,0),
           tl.col = "black",  cl.ratio = 0.5, cl.length=5, tl.offset=0,title = "Weather and Trip Variable Correlation")

cor(x=test[,c(21:30)],y=test[,c(72:82)],use="pairwise.complete.obs") %>%
  corrplot(show.diag=NULL, method = "color", type="upper", mar=c(0,0,2,0),
           tl.col = "black",  cl.ratio = 0.5, cl.length=5, tl.offset=0,title = "Weather and Trip Variable Correlation")

cor(x=test[,c(31:40)],y=test[,c(72:82)],use="pairwise.complete.obs") %>%
  corrplot(show.diag=NULL, method = "color", type="upper", mar=c(0,0,2,0),
           tl.col = "black",  cl.ratio = 0.5, cl.length=5, tl.offset=0,title = "Weather and Trip Variable Correlation")

cor(x=test[,c(41:50)],y=test[,c(72:82)],use="pairwise.complete.obs") %>%
  corrplot(show.diag=NULL, method = "color", type="upper", mar=c(0,0,2,0),
           tl.col = "black",  cl.ratio = 0.5, cl.length=5, tl.offset=0,title = "Weather and Trip Variable Correlation")

cor(x=test[,c(51:60)],y=test[,c(72:82)],use="pairwise.complete.obs") %>%
  corrplot(show.diag=NULL, method = "color", type="upper", mar=c(0,0,2,0),
           tl.col = "black",  cl.ratio = 0.5, cl.length=5, tl.offset=0,title = "Weather and Trip Variable Correlation")

cor(x=test[,c(61:65)],y=test[,c(72:82)],use="pairwise.complete.obs") %>%
  corrplot(show.diag=NULL, method = "color", type="upper", mar=c(0,0,2,0),
           tl.col = "black",  cl.ratio = 0.5, cl.length=5, tl.offset=0,title = "Weather and Trip Variable Correlation")

cor(x=test[,c(66:71)],y=test[,c(72:82)],use="pairwise.complete.obs") %>%
  corrplot(show.diag=NULL, method = "color", type="upper", mar=c(0,0,2,0),
           tl.col = "black",  cl.ratio = 0.5, cl.length=5, tl.offset=0,title = "Weather and Trip Variable Correlation")


