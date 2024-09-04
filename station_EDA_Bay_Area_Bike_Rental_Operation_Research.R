# Station EDA

# 1. Look at the data, get a sense of what it is like

# 2. Perform an EDA on a copy of the data
# a) Install tidyverse, funModeling, and Hmisc
library(tidyverse)
library(funModeling)
library(Hmisc)

# b) Examine data, make sure there is enough for suitable analysis, make sure everything is the right form

RawStation <- read.csv("station.csv")
summary(RawStation)
sum(as.character(RawStation$id) == "")
sum(as.character(RawStation$name) == "")
sum(as.character(RawStation$lat) == "")
sum(as.character(RawStation$long) == "")
sum(as.character(RawStation$dock_count) == "")
sum(as.character(RawStation$city) == "")
sum(as.character(RawStation$installation_date) == "")
#No missing values that aren't coded as NA

# c) Clean a copy of the data, including removing outliers - note what was changed

CleanedStation <- RawStation
# SK Id columns typically are unique for each row and factoring them is not helpful.
CleanedStation$id <- factor(CleanedStation$id)
# SK Same as above. Station names are expected to be unique.
CleanedStation$name <- factor(CleanedStation$name)
CleanedStation$city <- factor(CleanedStation$city)
CleanedStation$installation_date <- strptime(CleanedStation$installation_date, format = "%m/%d/%Y")

# d) Analyze categorical variables: do the categories make sense? Missingness?
summary(CleanedStation$id)
summary(CleanedStation$name)
summary(CleanedStation$city)
summary(CleanedStation$installation_date)
#No missingness

# e) Analyze numerical variables: outliers? What is the distribution of data? What are the standard deviations?
summary(CleanedStation$lat)

summary(CleanedStation$long)

summary(CleanedStation$dock_count)

ggplot(CleanedStation, mapping = aes(x=lat, y=long, col = dock_count)) + geom_point() + scale_colour_gradient()
# This maps out the stations by latitude and longitude, colour-coded by their number of docks

# It doesn't make sense to remove outliers given these are descriptive counting numbers: the latitude, longitude, number of docks, and are unlikely to reflect an error in measurement. Even if there is a higher-than-average number of docks, there is no clear reason as to why it should be excluded, or how it will negatively affect the analysis 
# SK You are correct. These are descriptives and as long as they all fall in the expected geographical
# region, they are not outliers. I would be worried if there was a dock in Toronto!
# f) Analyze categorical and numerical variables at the same time: get a sense of all the variables to be analyzed
summary(CleanedStation)
