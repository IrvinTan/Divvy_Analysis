# Google Data Analytics Professional Certificate
# Capstone Project
# Divvy Data Analysis

library(tidyverse)
library(lubridate)
library(ggplot2)

# Import and load the data
trip_202304 <- read.csv("202304-divvy-tripdata.csv")
trip_202303 <- read.csv("202303-divvy-tripdata.csv")
trip_202302 <- read.csv("202302-divvy-tripdata.csv")
trip_202301 <- read.csv("202301-divvy-tripdata.csv")
trip_202212 <- read.csv("202212-divvy-tripdata.csv")
trip_202211 <- read.csv("202211-divvy-tripdata.csv")
trip_202210 <- read.csv("202210-divvy-tripdata.csv")
trip_202209 <- read.csv("202209-divvy-publictripdata.csv")
trip_202208 <- read.csv("202208-divvy-tripdata.csv")
trip_202207 <- read.csv("202207-divvy-tripdata.csv")
trip_202206 <- read.csv("202206-divvy-tripdata.csv")
trip_202205 <- read.csv("202205-divvy-tripdata.csv")

# Merge the data
all_trips <- dplyr::bind_rows(trip_202304, trip_202303, trip_202302, trip_202301, 
                       trip_202212, trip_202211, trip_202210, trip_202209,
                       trip_202207, trip_202206, trip_202205)

# Add dates columns to the aggregate data
all_trips$date <- as.Date(all_trips$started_at) # The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Add ride_length column
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

# Convert ride_length to numeric for calculations on the data
all_trips$ride_length <- as.numeric(all_trips$ride_length)

# Clean data for bad data (zero or negative ride_length)
all_trips_clean <- filter(all_trips, ride_length > 0)

# Compare member and casual users across various data points
aggregate(all_trips_clean$ride_length ~ all_trips_clean$member_casual, FUN = mean)
aggregate(all_trips_clean$ride_length ~ all_trips_clean$member_casual, FUN = median)
aggregate(all_trips_clean$ride_length ~ all_trips_clean$member_casual, FUN = max)
aggregate(all_trips_clean$ride_length ~ all_trips_clean$member_casual, FUN = min)

# Compare member and casual users average ride time
aggregate(all_trips_clean$ride_length ~ all_trips_clean$member_casual + all_trips_clean$day_of_week, FUN = mean)

# Reorder the days of the week
all_trips_clean$day_of_week <- ordered(all_trips_clean$day_of_week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Compare member and casual users in number_of_rides and average_duration table
all_trips_clean %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday)

# Visualize number of rides for casual and member users
all_trips_clean %>% 
  mutate(weekday = wday(started_at, label = TRUE, week_start = 1)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Visualize average length of rides for casual and member users
all_trips_clean %>% 
  mutate(weekday = wday(started_at, label = TRUE, week_start = 1)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")


#colnames(all_trips)
#nrow(all_trips)
#dim(all_trips)
#head(all_trips)
#str(all_trips)
#summary(all_trips)

# Export all_trips_clean data
write.csv(all_trips_clean, "all_trips.csv")

# Export number_of_rides data
number_of_rides <- aggregate(all_trips_clean$day_of_week, by=list(all_trips_clean$member_casual, all_trips_clean$day_of_week), FUN = length)
write.csv(number_of_rides, "number_of_rides.csv")

# Export avg_ride_length data
avg_ride_length <- aggregate(all_trips_clean$ride_length ~ all_trips_clean$member_casual + all_trips_clean$day_of_week, FUN = mean)
write.csv(avg_ride_length, "avg_ride_length.csv")

