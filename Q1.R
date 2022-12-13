rm(list = ls())
set.seed(1234567890)
# Q1 ----------
library(geosphere)
stations <- read.csv("./stations.csv", fileEncoding = "latin1")
temps50k <- read.csv("./temps50k.csv")
data_full <- merge(stations, temps50k, by = "station_number")



#choose target location 
set.seed(1378)
# target_station <- sample(stations$station_number, 1)
target_station <- 84260

target_loc <- stations[stations$station_number == target_station, c("longitude", "latitude")]
target_data <- data_full[data_full == target_station, ]
target_date <- "2013-11-04"

hours <- seq(as.POSIXct("2013-11-04 04:00"), as.POSIXct("2013-11-04 24:00"), by = "2 hour")
hours <- format(hours, format = "%H:%M:%S")

temp <- vector(length = length(hours))

max_distance <- max(distHaversine(target_loc, data_full[,  c("longitude", "latitude")]))
# h_distance <- 10 # These three values are up to the students
# h_date <- 10
# h_time <- 10

h_distance <- 500; h_date <- 1000; h_time <- 0.5


for(i in seq_along(hours)){
  available_data <- data_full[(
    # data_full$station_number != target_station &
    data_full$time <= hours[i] &
      data_full$date <= target_date
  ), ]
  
  dist_1 <- distHaversine(target_loc, available_data[,  c("longitude", "latitude")])
  # plot(dist_1, exp(-dist_1/(2*h_distance^2)))
  # plot(dist_1/(2*h_distance^2), exp(-dist_1/(2*h_distance^2)), type = "l")
  # plot(exp(-dist_1/(2*h_distance^2)), type = "l")
  
  dist_2 <- difftime(target_date, available_data$date, units = "day")
  # plot(dist_2, exp(-as.numeric(dist_2)/h_date))
  dist_3 <- difftime(as.POSIXct(hours[i], format = "%H:%M:%S"),
                     as.POSIXct(available_data$time, format = "%H:%M:%S"),
                     units = "hour")
  # plot(dist_3, exp(-as.numeric(dist_3)/h_time))
  
  weight <- exp(-dist_1/h_distance) + exp(-as.numeric(dist_2)/h_date) + exp(-as.numeric(dist_3)/h_time)
  temp[i] <- sum(available_data$air_temperature * weight) / sum(weight)
}



plot(temp, type="o")



for(i in seq_along(hours)){
  available_data <- data_full[(data_full$station_number != target_station &
                                 data_full$time <= hours[i] &
                                 data_full$date <= target_date)
                              , ]
  
  dist_1 <- distHaversine(target_loc, available_data[,  c("longitude", "latitude")])
  # plot(dist_1, exp(-dist_1/(2*h_distance^2)))
  # plot(dist_1/(2*h_distance^2), exp(-dist_1/(2*h_distance^2)), type = "l")
  # plot(exp(-dist_1/(2*h_distance^2)), type = "l")
  # TODO: units
  dist_2 <- difftime(target_date, available_data$date, units = "day")
  # plot(dist_2, exp(-as.numeric(dist_2)/h_date))
  dist_3 <- difftime(as.POSIXct(hours[i], format = "%H:%M:%S"),
                     as.POSIXct(available_data$time, format = "%H:%M:%S"),
                     units = "hour")
  # plot(dist_3, exp(-as.numeric(dist_3)/h_time))
  
  weight <- exp(-dist_1/h_distance) * exp(-as.numeric(dist_2)/h_date) * exp(-as.numeric(dist_3)/h_time)
  temp[i] <- sum(available_data$air_temperature * weight) / sum(weight)
}


plot(temp, type="o")

