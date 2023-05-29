# Required packages
library(tidyverse)
library(R6)


# Define the monorail system parameters
num_trains <- 3
num_stations <- 2
track_length <- 1000
max_capacity <- 100
simulation_time <- 50
default_load_time <- 3  # Default load time in minutes

# Define a class to represent a monorail train
Train <- R6Class("Train",
                 public = list(
                   position = NULL,
                   speed = NULL,
                   direction = NULL,
                   capacity = NULL,
                   passengers_moved = 0,
                   initialize = function() {
                     self$position <- 0
                     self$speed <- 0
                     self$direction <- 1  # 1 for forward, -1 for backward
                     self$capacity <- max_capacity
                   },
                   move = function() {
                     self$position <- self$position + (self$speed * self$direction)
                   },
                   accelerate = function() {
                     self$speed <- min(self$speed + 1, 5)  # Limit maximum speed to 5
                   },
                   decelerate = function() {
                     self$speed <- max(self$speed - 1, 0)  # Prevent negative speed
                   },
                   load_passengers = function(passengers) {
                     self$capacity <- max(self$capacity - passengers, 0)
                   },
                   unload_passengers = function(passengers) {
                     self$capacity <- min(self$capacity + passengers, max_capacity)
                   },
                   update_passengers_moved = function(passengers) {
                     self$passengers_moved <- self$passengers_moved + passengers
                   }
                 )
)

# Define a class to represent a monorail station
Station <- R6Class("Station",
                   public = list(
                     position = NULL,
                     passengers = NULL,
                     load_time = NULL,
                     initialize = function(load_time = default_load_time) {
                       self$position <- 0
                       self$passengers <- 0
                       self$load_time <- load_time
                     },
                     add_passengers = function(num_passengers) {
                       self$passengers <- self$passengers + num_passengers
                     },
                     remove_passengers = function(num_passengers) {
                       self$passengers <- max(self$passengers - num_passengers, 0)
                     }
                   )
)

# Create monorail system objects
trains <- vector("list", num_trains)
for (i in 1:num_trains) {
  trains[[i]] <- Train$new()
}

stations <- vector("list", num_stations)
for (i in 1:num_stations) {
  stations[[i]] <- Station$new()
}

# Simulation loop
total_passengers_moved <- 0  # Variable to track total passengers moved
for (t in 1:simulation_time) {
  # Print current system state
  cat("Time:", t, "\n")
  for (i in 1:num_trains) {
    train <- trains[[i]]
    cat("Train", i, "- Position:", train$position, "Speed:", train$speed, "Direction:", train$direction, "Capacity:", train$capacity, "\n")
  }
  cat("\n")
  
  # Train movement
  for (i in 1:num_trains) {
    train <- trains[[i]]
    train$move()
    
    # Reverse direction if train reaches the end of the track
    if (train$position >= track_length || train$position <= 0) {
      train$direction <- -train$direction
    }
    
    # Adjust train speed based on station passenger load
    station_index <- min(max(floor((train$position - 1) / (track_length / num_stations)) + 1, 1), num_stations)
    station <- stations[[station_index]]
    
    if (station$passengers > 0 && train$speed == 0) {
      train$accelerate()
    } else if (station$passengers == 0 && train$speed > 0) {
      train$decelerate()
    }
    
    # Load and unload passengers at the station
    if (station$passengers > 0 && t %% (station$load_time * 60) == 0) {
      passengers_to_unload <- min(train$capacity, station$passengers)
      passengers_unloaded <- min(passengers_to_unload, train$capacity - train$passengers_moved)
      train$unload_passengers(passengers_unloaded)
      station$remove_passengers(passengers_unloaded)
      
      passengers_to_load <- min(train$capacity, station$passengers)
      train$load_passengers(passengers_to_load)
      station$remove_passengers(passengers_to_load)
      
      train$update_passengers_moved(passengers_unloaded)
      total_passengers_moved <- total_passengers_moved + passengers_unloaded
    }
  }
  
  # Station passenger generation (random for demonstration purposes)
  for (i in 1:num_stations) {
    stations[[i]]$add_passengers(sample(0:10, 1))
  }
}

# Print total passengers moved
cat("Total passengers moved:", total_passengers_moved, "\n")

# Visualization (using ggplot2 for demonstration purposes)
train_data <- data.frame(Time = rep(1:simulation_time, num_trains),
                         Train = rep(1:num_trains, each = simulation_time),
                         Position = sapply(trains, function(train) {
                           if (train$direction == 1) {
                             train$position %% (track_length / num_stations)
                           } else {
                             (track_length / num_stations) - (train$position %% (track_length / num_stations))
                           }
                         }))




ggplot(train_data, aes(x = Time, y = Position, group = Train, color = Train)) +
  geom_line() +
  labs(x = "Time", y = "Train Position")


train_data %>%
  filter(Train == 1) %>%
  ggplot(aes(x = Time, y = Position, group = Train)) +
  geom_line() +
  labs(x = "Time", y = "Train Position (Distance)") +
  scale_y_continuous(labels = function(x) paste(x, "m")) +
  theme_bw()

