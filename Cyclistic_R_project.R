# 1. Primary steps ------------
## 1.1 libraries ------------

#Get the libraries needed 

library(tidyverse)
library(janitor)
library(skimr)
library(lubridate)
library(leaflet)

## 1.2 House cleaning-----------
rm(list = ls())


# 2. Prepare ----------------
## 2.1 Getting data ------------

# let us load the data from the last 12 months in 2022 

dec_2022 <- read.csv("Raw_Data/202212-divvy-tripdata.csv")
nov_2022 <- read.csv("Raw_Data/202211-divvy-tripdata.csv")
oct_2022 <- read.csv("Raw_Data/202210-divvy-tripdata.csv")
sep_2022 <- read.csv("Raw_Data/202209-divvy-publictripdata.csv")
aug_2022 <- read.csv("Raw_Data/202208-divvy-tripdata.csv")
july_2022 <- read.csv("Raw_Data/202207-divvy-tripdata.csv")
june_2022 <- read.csv("Raw_Data/202206-divvy-tripdata.csv")
may_2022 <- read.csv("Raw_Data/202205-divvy-tripdata.csv")
april_2022 <- read.csv("Raw_Data/202204-divvy-tripdata.csv")
march_2022 <- read.csv("Raw_Data/202203-divvy-tripdata.csv")
feb_2022  <- read.csv("Raw_Data/202202-divvy-tripdata.csv")
jan_2022 <- read.csv("Raw_Data/202201-divvy-tripdata.csv")

## 2.2 Compare data ----------------
# make sure the data sets are of same type
compared_data <- compare_df_cols(jan_2022, feb_2022, march_2022, april_2022, may_2022, june_2022, july_2022, aug_2022, sep_2022, 
                oct_2022, nov_2022, dec_2022, return = "mismatch")

print(compared_data)

## 2.3 Combine data -----------------

trip_data <- bind_rows(jan_2022, feb_2022, march_2022, april_2022, may_2022, june_2022, 
                       july_2022, aug_2022, sep_2022, oct_2022, nov_2022, dec_2022)

dim(trip_data)

# remove empty rows and columns 
trip_data <- remove_empty(trip_data, which = c("rows", "cols"))

dim(trip_data)
head(trip_data)

## 2.4 Summary of the data ---------------
skim_without_charts(trip_data)

## process the datetime
trip_data$started_at <- ymd_hms(trip_data$started_at)
trip_data$ended_at <- ymd_hms(trip_data$ended_at)

# see the structure of the data 
#str(trip_data)

## 2.5 Ride duration ------------

#### let us create ride length field
trip_data$ride_length_sec <- difftime(trip_data$ended_at, 
                                      trip_data$started_at,
                                      units = "secs")

trip_data$ride_length_min <- difftime(trip_data$ended_at, 
                                      trip_data$started_at,
                                      units = "mins")
## also create separate columns for year, month, week and day-of-the-week etc

trip_data$year <- year(trip_data$started_at)
trip_data$month <- month(trip_data$started_at)
trip_data$month_letter <- month(trip_data$started_at, abbr = TRUE, label = TRUE)
trip_data$week <- week(trip_data$started_at)

# create day of the month column
trip_data$day <- day(trip_data$started_at)

# create weekdays
trip_data$weekday_letter <- wday(trip_data$started_at, abbr = TRUE, label = TRUE)
trip_data$weekday_number <- wday(trip_data$started_at)


### summary of the data 
skim_without_charts(trip_data)


# 3. Cleaning -----------

## 3.1 remove NA ------------
trip_data_cleaned <- drop_na(trip_data)

## 3.2 remove ride_length <0 ----------------
trip_data_cleaned <- trip_data_cleaned %>% filter(ride_length_min>=0)

## 3.3 remove duplicate --------------
trip_data_cleaned <- distinct(trip_data_cleaned)


## 3.4 Remove extra dataset ----------
rm(jan_2022, feb_2022, march_2022, april_2022, may_2022, june_2022, 
            july_2022, aug_2022, sep_2022, oct_2022, nov_2022, dec_2022)
rm(compared_data)

## 3.4 creat map_data to see in Tableau
# to check if there are outliers on the map
map_data <- trip_data_cleaned %>% 
  select(
    start_station_name,
    start_lat,
    start_lng,
    member_casual) %>% 
  
  group_by(start_station_name) %>% 
  
  mutate(
    numtrips = n()
  ) %>% 
  
  distinct(
    start_station_name,
    .keep_all = TRUE
  )

# 4. Save the clean data ----------

# save the cleaned trip_Data
#write.csv(trip_data_cleaned, "cyclistic_data_cleaned.csv")


# save the map_data
write.csv(map_data, "cyclistic_map_data.csv")