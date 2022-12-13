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






# This part is to check if the weight is suitable for our application
h_distance <- 50000 ; h_date <- 365/2; h_time <- 4  # These values are for testing

# kernel of distance
max_distance <- max(distHaversine(target_loc, data_full[,  c("longitude", "latitude")])) #find the max difference between the target station and the other station
distance <- seq(0,max_distance,1)/h_distance
k_dist <- exp(-(distance)^2)
distance_kernal_plot <- plot(k_dist, type = "l", xlab = "Distance", ylab = "kernel") 

distance_kernal_plot_xlim <- plot(k_dist, type = "l", xlab = "Distance", ylab = "kernel",xlim=c(0,200000)) 
# Since some of the other stations are too far away, limit the x-value to make observation easier
distance_kernal_plot_xlim


# kernel of date
max_date_diff <- max(as.vector(difftime(target_date,data_full$date, units = "day"))) #this will gives the maxinmu date difference to "previous date"

#Here, calculate the max/min difference of date with target date, this doesn't account for season since we only calculate the true difference of the date-
date <- seq(0,max_date_diff,1)/h_date
k_date <- exp(-(date)^2)
plot(k_date, type= "l", xlab= "Date", ylab = "kernel",xlim=c(0,1500)) # we only present t closest 2000 day, since the day that is before that has too little affect to the vaule, which is also what we want


# kernel of time
time <- seq(-12,12,1)/h_time
k_time <- exp(-(time)^2)
plot(k_time, xlab = "Time", ylab = "kernel",xlim=c(0,24), type = "l")


#Computing
#Summation Kernel

temperature_sum <- vector(length = length(hours))
for(i in seq_along(hours)){
  available_data <- data_full[(
    # data_full$station_number != target_station &
    data_full$time <= hours[i] &
      data_full$date <= target_date
  ), ]
  
  dist_1 <- distHaversine(target_loc, available_data[,  c("longitude", "latitude")])

  
  
  dist_2 <- difftime(target_date, available_data$date, units = "day")
  # plot(dist_2, exp(-as.numeric(dist_2)/h_date))
  dist_3 <- difftime(as.POSIXct(hours[i], format = "%H:%M:%S"),
                     as.POSIXct(available_data$time, format = "%H:%M:%S"),
                     units = "hour")
  # plot(dist_3, exp(-as.numeric(dist_3)/h_time))
  
  weight <- exp(-dist_1/h_distance) + exp(-as.numeric(dist_2)/h_date) + exp(-as.numeric(dist_3)/h_time)
  temperature_sum[i] <- sum(available_data$air_temperature * weight) / sum(weight)
}

#Multiplying Kernel

temperature_multiply <- vector(length = length(hours))
for(i in seq_along(hours)){
  available_data <- data_full[(
    # data_full$station_number != target_station &
    data_full$time <= hours[i] &
      data_full$date <= target_date
  ), ]
  
  dist_1 <- distHaversine(target_loc, available_data[,  c("longitude", "latitude")])
  
  
  
  dist_2 <- difftime(target_date, available_data$date, units = "day")
  # plot(dist_2, exp(-as.numeric(dist_2)/h_date))
  dist_3 <- difftime(as.POSIXct(hours[i], format = "%H:%M:%S"),
                     as.POSIXct(available_data$time, format = "%H:%M:%S"),
                     units = "hour")
  # plot(dist_3, exp(-as.numeric(dist_3)/h_time))
  
  weight <- exp(-dist_1/h_distance) * exp(-as.numeric(dist_2)/h_date) * exp(-as.numeric(dist_3)/h_time)
  temperature_multiply[i] <- sum(available_data$air_temperature * weight) / sum(weight)
}


# hours[11] <- "24:00:00"
library(ggplot2)# Not sure if we can use ggplot
temperature_sum <- data.frame(cbind(hours,temperature_sum))
temperature_sum[11,1] <- "24:00:00"# change the title so the ploting will have 2400 at the right hand side

plotdata <- data.frame(cbind(temperature_sum,temperature_multiply))
ggplot(data =plotdata)+
  geom_line(aes(x=hours,y=as.numeric(temperature_sum),group=1,colour="Summation"))+
  geom_line(aes(x=hours,y=as.numeric(temperature_multiply),group=1 ,colour="Multiplying"))+
  scale_color_manual(name = "Y series", values = c("Summation" = "darkblue", "Multiplying" = "red"))+
  ylab("Temperature")+
  ggtitle("Comparesion of Sumatinon and Multiplying kernels")
  

  