# Bay_Area_Bike_Rental_Operation_Research
A repo containing code for the Bay Area Bike Rental Operation Research project

#Plan
1. Look at the data, get a sense of what it is like
2. Perform an EDA on a copy of the data
   a) Install tidyverse, funModeling, and Hmisc
   b) Examine data, make sure there is enough for suitable analysis, make sure everything is the right form
   c) Clean a copy of the data, including removing outliers - note what was changed
   d) Analyze categorical variables: do the categories make sense? Missingness?
   e) Analyze numerical variables: outliers? What is the distribution of data? What are the standard deviations?
   f) Analyze categorical and numerical variables at the same time: get a sense of all the variables to be analyzed
-------------------------------------------------------------------------------
3. Record trip IDs for outliers, and then remove them. What are the rules for outliers? Outside of 95% of the data? Or a different metric?
4. Record and remove "cancelled trips" (i.e., any trip <3 minutes)
5. Determine weekday rush hours - What consists of rush hour? Provide a definition. Histograms by hour are a good idea to test, or 15 minutes to be more precise. Lubridate will be useful for determining time intervals
6. Determine 10 most frequent starting and ending stations during rush hours
7. Determine 10 most frequent starting and ending stations during the weekends
8. Determine the average utilization of bikes for each month (total time bikes were used/total time in the month)
-------------------------------------------------------------------------------
9. Create a new dataset combining trip data with weather data. Weather data is available for each city and date. The cor function from the Corrplot package will be useful for creating a correlation matrix. Deal with the fact that it is difficult to correlate categorical and non-categorical variables. Some trips start and end in different cities, will need to explain how to correlate weather data with trip data if the trip is crossing cities - may have implications for how the two datasets are joined





