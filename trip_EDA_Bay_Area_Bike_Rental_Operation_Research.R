# Trip EDA

# 1. Look at the data, get a sense of what it is like

# 2. Perform an EDA on a copy of the data
# a) Install tidyverse, funModeling, and Hmisc
library(tidyverse)
library(funModeling)
library(Hmisc)

# b) Examine data, make sure there is enough for suitable analysis, make sure everything is the right form

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

# c) Clean a copy of the data, including removing outliers - note what was changed

CleanedTrip <- RawTrip
# SK Id columns typically are unique for each row and factoring them is not helpful.
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

# d) Analyze categorical variables: do the categories make sense? Missingness?

# there is no missingness in the categorical variables

# e) Analyze numerical variables: outliers? What is the distribution of data? What are the standard deviations?

plot(CleanedTrip$start_date,CleanedTrip$duration)
# Clear outlier of 17270400
difftime(CleanedTrip[229947,6],CleanedTrip[229947,2], units = c("weeks"))
# This outputs 1417921080 secs, or 23632018 mins, or 393867 hours, or 2344.446 weeks. This is an incredibly long time for a bike to be out, but it also does not clarify the units of the 17270400 measure, nor does it explain why the numbers are different, or what "duration" refers to
OutlierTrip <- CleanedTrip[229947,]
CleanedTrip <- CleanedTrip[-229947,]
plot(CleanedTrip$start_date,CleanedTrip$duration)
PotentialOutliers <- CleanedTrip[which(CleanedTrip$duration>quantile(CleanedTrip$duration,probs=0.999)),]
# Potential outliers are approximately >20 hours apart, though once again, the duration is not clearly linked to the start and end times

CleanedTrip <- mutate(CleanedTrip, TripDuration = (CleanedTrip$end_date-CleanedTrip$start_date))
# The longest trip is ~8 hours 20 minutes
RearrangedTripDuration <- arrange(CleanedTrip,desc(duration))
plot(RearrangedTripDuration$TripDuration) #the longest duration trips are part of a small minority
TripDurationOutlierIndices <- which(CleanedTrip$TripDuration>quantile(CleanedTrip$TripDuration,probs=0.999))
TripDurationOutliers <- CleanedTrip[TripDurationOutlierIndices,]
# the 99.9th percentile is approximately 20 hours long. This seems to be a reasonable cutoff for trip lengths, as it is approaching a full day - Bay Area bike rental lengths usually go up to full-day or 24 hours (though there are some that can be rented on a weekly, or even monthly basis) - https://www.lyft.com/bikes/bay-wheels/sf-bike-rental
CleanedTrip <- CleanedTrip[-TripDurationOutlierIndices,] #remove the outlier indices from the main cleaned dataframe



# f) Analyze categorical and numerical variables at the same time: get a sense of all the variables to be analyzed

# SK good job with the outlier analysis.
# (Points taken) However the output of the EDA above shows 70 unique start/end station IDs, and 
# 74 unique start/end station names. This is a discrepancy worth looking into.

