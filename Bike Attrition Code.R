# Downloaded and installed tidyverse packages
install.packages("tidyverse")
library(tidyverse)

# After importing csv files, renamed them so they are easier to interact with
q1_2020 <- read_csv("Divvy_Trips_2020_Q1_R_studio_version.csv")
q1_2019 <- read_csv("Divvy_Trips_2019_Q1_R_studio_version.csv")

# Checked heading of each spreadsheet to make sure they match
colnames(q1_2019)
colnames(q1_2020)

# Renamed certain headings that didn't match so the sheets can be joined
(q1_2019 <- rename(q1_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid
                   ,started_at = start_time
                   ,ended_at = end_time
                   ,start_station_name = from_station_name
                   ,start_station_id = from_station_id
                   ,end_station_name = to_station_name
                   ,end_station_id = to_station_id
                   ,member_casual = usertype
))

# Double checked the column names to make sure they are the same after joining
colnames(q1_2019)
colnames(q1_2020)

# Inspected the dataframes and looked for incongruencies
str(q1_2019)
str(q1_2020)

# Converted ride_id and rideable_type to character so that they can stack correctly
q1_2019 <-  mutate(q1_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))

# Converted ride_length to character in both data frames
q1_2019 <- mutate(q1_2019, ride_length = as.character(ride_length))
q1_2020 <- mutate(q1_2020, ride_length = as.character(ride_length))

# Stacked individual quarter's data frames into one big data frame named 'all_trips'
all_trips <- bind_rows(q1_2019, q1_2020)#, q3_2019)#, q4_2019, q1_2020)

# Checked the column types before combining, run
glimpse(q1_2019)
glimpse(q1_2020)

# Removed lat, long, birthyear, and gender fields as this data was dropped beginning in 2020
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender,  "tripduration"))

# Inspected the dataframe and looked for incongruencies
str(all_trips)

# In the "member_casual" column, replaced "Subscriber" with "member" and "Customer" with "casual"
all_trips <- all_trips %>%
  mutate(member_casual = case_when(
    member_casual == "Subscriber" ~ "member",
    member_casual == "Customer" ~ "casual",
    TRUE ~ member_casual  # leave other values unchanged
  ))

# Convert "ride_length" from character to numeric so we can run calculations on the data
library(lubridate)
# started_at: "1/1/19 12:04 AM" → m/d/y with AM/PM
all_trips$started_at <- mdy_hms(all_trips$started_at, tz = "UTC", truncated = 1)
# ended_at: "1/1/2019 0:11" → m/d/Y H:M (24-hour clock, no seconds)
all_trips$ended_at <- mdy_hm(all_trips$ended_at, tz = "UTC")
# Parse and convert to duration in seconds
all_trips$ride_length <- as.numeric(difftime(all_trips$ended_at, all_trips$started_at, units = "mins"))

# Adds columns that list the date, month, day, and year of each ride
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Inspect the structure of the columns
str(all_trips)

# Removes "bad" data, the dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

# Descriptive analysis on ride_length
mean(all_trips_v2$ride_length, na.rm = TRUE) #straight average (total ride length / rides)
median(all_trips_v2$ride_length, na.rm = TRUE) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length, na.rm = TRUE) #longest ride
min(all_trips_v2$ride_length, na.rm = TRUE) #shortest ride

# Compares members and casual users
aggregate(all_trips$ride_length ~ all_trips$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# The days of the week are store and categorized numerically this allows them to be identified by their names
all_trips$day_of_week <- recode(all_trips$day_of_week,
                                "1" = "Sunday",
                                "2" = "Monday",
                                "3" = "Tuesday",
                                "4" = "Wednesday",
                                "5" = "Thursday",
                                "6" = "Friday",
                                "7" = "Saturday"
)

# The days of the week were out of order
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Inspected the structure of the columns again
str(all_trips)

# Shows the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# Analyzed ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)		

# Let's visualize the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Let's create a visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

write.csv(counts, file = "Bike Case Study.csv", row.names = FALSE)

counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = 'avg_ride_length.csv')
