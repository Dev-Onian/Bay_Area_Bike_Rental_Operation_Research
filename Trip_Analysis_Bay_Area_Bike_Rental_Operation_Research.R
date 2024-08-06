# Trip_Analysis
library(tidyverse)
library(funModeling)
library(Hmisc)
library(dplyr)
library(lubridate)
library(stringr)
library(mctq)
library(tidyr)
library(ivs)

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
RushHourCutoff <- quantile(RushHours$NumberOfTrips,probs=0.75)

RushHours$Interval <- str_remove_all(RushHours$Interval,"\\d{4}[-]\\d{2}[-]\\d{2}")
RushHours$Interval <- str_remove_all(RushHours$Interval,"[:]\\d{2}[ ]")
RushHours$Interval[1] <- "00:00EST--00:15EST"
RushHours$Interval[96] <- "23:45EST--00:00EST"

cols <- c("white", "red")[(RushHours$NumberOfTrips > RushHourCutoff) + 1]  
par(mar=c(6,4,4,1))
barplot(RushHours$NumberOfTrips, col = cols, names.arg=unlist(RushHours$Interval),las=2,
        ylab = "Number of Trips",
        main = "Bike Usage Stratified By Time",
        cex.names=0.5)


# 6. Determine 10 most frequent starting and ending stations during rush hours

RushHourTripIndices <- int_overlaps(RushHourTimes$POSIXInterval[1],CleanedTrip$ShiftedTripInterval)
for (i in 1:nrow(RushHourTimes)){
  RushHourTripIndices <- append(RushHourTripIndices,int_overlaps(RushHourTimes$POSIXInterval[i],CleanedTrip$ShiftedTripInterval))
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

MonthDurations <- as.numeric(MonthDurations)
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
plot(OverallBikeUsage)

