set.seed(1234567890)
# Q1 ----------
library(geosphere)
stations <- read.csv("./stations.csv", fileEncoding = "latin1")
temps50k <- read.csv("./temps50k.csv")

data_full <- merge(stations, temps50k, by = "station_number")


set.seed(1378)
target_station <- sample(stations$station_number, 1)
target_station <- 84260
target_loc <- stations[stations$station_number == target_station, c("longitude", "latitude")]


target_data <- data_full[data_full == target_station, ]


target_date <- "2013-11-04"
hours <- seq(as.POSIXct("2013-11-04 04:00"), as.POSIXct("2013-11-04 24:00"), by = "2 hour")
hours <- format(hours, format = "%H:%M:%S")

temp <- vector(length = length(hours))


h_distance <- 10 # These three values are up to the students
h_date <- 10
h_time <- 10

h_distance <- 500; h_date <- 1000; h_time <- 0.5


for(i in seq_along(hours)){
  available_data <- data_full[(
    # data_full$station_number != target_station &
      data_full$time <= hours[i] &
      data_full$date <= target_date
    ), ]
  
  dist_1 <- distHaversine(target_loc, available_data[,  c("longitude", "latitude")])
  plot(dist_1, exp(-dist_1/(2*h_distance^2)))
  plot(dist_1/(2*h_distance^2), exp(-dist_1/(2*h_distance^2)), type = "l")
  plot(exp(-dist_1/(2*h_distance^2)), type = "l")
  # TODO: units
  dist_2 <- difftime(target_date, available_data$date, units = "day")
  plot(dist_2, exp(-as.numeric(dist_2)/h_date))
  dist_3 <- difftime(as.POSIXct(hours[i], format = "%H:%M:%S"),
                     as.POSIXct(available_data$time, format = "%H:%M:%S"),
                     units = "hour")
  plot(dist_3, exp(-as.numeric(dist_3)/h_time))
  
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
  plot(dist_1, exp(-dist_1/(2*h_distance^2)))
  plot(dist_1/(2*h_distance^2), exp(-dist_1/(2*h_distance^2)), type = "l")
  plot(exp(-dist_1/(2*h_distance^2)), type = "l")
  # TODO: units
  dist_2 <- difftime(target_date, available_data$date, units = "day")
  plot(dist_2, exp(-as.numeric(dist_2)/h_date))
  dist_3 <- difftime(as.POSIXct(hours[i], format = "%H:%M:%S"),
                     as.POSIXct(available_data$time, format = "%H:%M:%S"),
                     units = "hour")
  plot(dist_3, exp(-as.numeric(dist_3)/h_time))
  
  weight <- exp(-dist_1/h_distance) * exp(-as.numeric(dist_2)/h_date) * exp(-as.numeric(dist_3)/h_time)
  temp[i] <- sum(available_data$air_temperature * weight) / sum(weight)
}


plot(temp, type="o")


# Q2 ----------
library(kernlab)
set.seed(1234567890)

data(spam)
foo <- sample(nrow(spam))
spam <- spam[foo, ]
spam[, -58] <- scale(spam[, -58])
tr <- spam[1:3000, ]
va <- spam[3001:3800, ]
trva <- spam[1:3800, ]
te <- spam[3801:4601, ] 

by <- 0.3
err_va <- NULL
for(i in seq(by ,5, by)){
  filter <- ksvm(type ~ ., data = tr, kernel = "rbfdot", kpar = list(sigma = 0.05), C = i, scaled = FALSE)
  mailtype <- predict(filter, va[, -58])
  t <- table(mailtype, va[, 58])
  err_va <- c(err_va, (t[1, 2] + t[2, 1]) / sum(t))
}

filter0 <- ksvm(type ~ ., data = tr, kernel = "rbfdot", kpar = list(sigma = 0.05), 
                C = which.min(err_va) * by,
                scaled = FALSE)
mailtype <- predict(filter0, va[, -58])
t <- table(mailtype, va[, 58])
err0 <- (t[1, 2] + t[2, 1]) / sum(t)
err0

filter1 <- ksvm(type ~ ., data = tr, kernel = "rbfdot", kpar = list(sigma = 0.05),
                C = which.min(err_va) * by,
                scaled = FALSE)
mailtype <- predict(filter1, te[, -58])
t <- table(mailtype, te[, 58])
err1 <- (t[1, 2] + t[2, 1]) / sum(t)
err1

filter2 <- ksvm(type ~ ., data = trva, kernel = "rbfdot", kpar = list(sigma = 0.05), 
                C = which.min(err_va) * by,
                scaled = FALSE)
mailtype <- predict(filter2, te[, -58])
t <- table(mailtype, te[, 58])
err2 <- (t[1, 2] + t[2, 1]) / sum(t)
err2

filter3 <- ksvm(type ~ ., data = spam, kernel = "rbfdot", kpar = list(sigma = 0.05),
                C = which.min(err_va) * by,
                scaled = FALSE)
mailtype <- predict(filter3, te[, -58])
t <- table(mailtype, te[, 58])
err3 <- (t[1, 2] + t[2, 1]) / sum(t)
err3

# Questions

# 1. Which filter do we return to the user ? filter0, filter1, filter2 or filter3? Why?
## f3 is worst since the test data is already used to train the model + err3<err0 !!
## f2 or f1 both are good. altho f2 is alittle bit better since the training data is bigger and it will help the model to generalize better

# 2. What is the estimate of the generalization error of the filter returned to the user? err0, err1, err2 or err3? Why?
## based on previous question err2

# 3. Implementation of SVM predictions.

sv <- alphaindex(filter3)[[1]]
co <- coef(filter3)[[1]]
inte <- -b(filter3)

k <- NULL
for (i in 1:10) {
  # We produce predictions for just the first 10 points in the dataset.
  x_star <- spam[i, -58]
  k2 <- NULL
  for (j in 1:length(sv)) {
    k2 <- c(k2,
            exp( -0.05 * sum( (x_star - spam[sv[j], -58])^2 ) )
    )
  }
  
  k <- c(k, sum(co * k2) + inte)
}
k
predict(filter3,spam[1:10,-58], type = "decision")



important_obs <- spam[sv, -58]
sv_mat <- as.matrix(important_obs)
for(i in 1:10){
  x_star <- unname(unlist(spam[i, -58]))
  dist <- colSums((x_star - t(sv_mat))^2)
  k <- exp(-dist * 0.05)
  print(sum(k*co) + inte)
}


# Q3 ----------------

## 3.1 ----------
library(neuralnet)
set.seed(1234567890)
Var <- runif(500, 0, 10)
mydata <- data.frame(Var, Sin = sin(Var))
tr <- mydata[1:25, ] # Training
te <- mydata[26:500, ] # Test
# Random initialization of the weights in the interval [-1, 1]
winit <- runif(31, -1, 1)
nn <- neuralnet(Sin~Var,
                tr,
                10,
                startweights = winit)
# Plot of the training data (black), test data (blue), and predictions (red)
plot(tr, cex = 2)
points(te, col = "blue", cex = 1)
points(te[, 1], predict(nn, te), col = "red", cex = 1)


plot(nn)
y_hat <- predict(nn, te)


## 3.2 ----------

h1 <- function(x) x
# Random initialization of the weights in the interval [-1, 1]
winit <- runif(20, -1, 1)
nn_h1 <- neuralnet(Sin~Var,
                tr,
                10,
                act.fct = h1,
                startweights = winit)
# Plot of the training data (black), test data (blue), and predictions (red)
plot(tr, cex = 2)
points(te, col = "blue", cex = 1)
points(te[, 1], predict(nn_h1, te), col = "red", cex = 1)

#TODO
h2 <- function(x) x / (1 + exp(-2 * 10000 * x))
nn_h2 <- neuralnet(Sin~Var,
                   tr,
                   10,
                   act.fct = h2,
                   startweights = winit)
# Plot of the training data (black), test data (blue), and predictions (red)
plot(tr, cex = 2)
points(te, col = "blue", cex = 1)
points(te[, 1], predict(nn_h2, te), col = "red", cex = 1)


h3 <- function(x) log(1 + exp(x))
nn_h3 <- neuralnet(Sin~Var,
                   tr,
                   10,
                   act.fct = h3,
                   startweights = winit)
# Plot of the training data (black), test data (blue), and predictions (red)
plot(tr, cex = 2)
points(te, col = "blue", cex = 1)
points(te[, 1], predict(nn_h3, te), col = "red", cex = 1)

## 3.3 -------
new_x <- runif(500, 0, 50)
new_df <- data.frame(Var = new_x, Sin = sin(new_x))
predict(nn, new_df)

plot(new_df[, 1], predict(nn, new_df), col = "red", cex = 1)
points(new_df, col = "blue", cex = 1)

## 3.4 -------
nn$weights
plot(nn)

## 3.5 ---------

new_x <- runif(500, 0, 10)
new_df <- data.frame(Var = new_x, Sin = sin(new_x))
nn_3_5 <- neuralnet(Sin~Var,
                tr,
                10,
                startweights = winit)

plot(predict(nn, new_df), new_df[, 2])
points(new_df, col = "blue")
