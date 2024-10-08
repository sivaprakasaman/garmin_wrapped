#Author: Andrew Sivaprakasam
#Last Updated: October 2024
#Description: This code helps split runs into useful distances for more fine-grained
# Analysis
#References: Many functions here made quickly with chatGPT assistance (albeit with many corrections including 
# distance conversion XD)

## Dependencies
## Installing Dependencies & Importing Libraries
list.of.packages <- c('ggplot2','remotes','dplyr','purrr','PerformanceAnalytics','nloptr','lubridate','revgeo','maps','gt')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(lubridate)
library(ggplot2)
library(ggpubr)
library(leaflet)
library(dplyr)
library(sf)

## TODO ## 
# - Script to split whole run into even, fixed segments (e.g. mi, km, 5km)
# - Script to split run into pre-defined arbitrary segments (e.g. 5k, 10k, HM)
# - Script to find fastest defined split (e.g. fastest 5k in a 10k or HM etc)

## Functions

# Function to calculate split times and identify splits
calculate_splits_constant <- function(distance_vector, time_vector, split_distance_str) {
  # Parse the split distance and unit
  split_distance <- as.numeric(gsub("[^0-9.]", "", split_distance_str))
  unit <- tolower(gsub("[0-9]", "", split_distance_str))
  # Convert split distance to miles if necessary
  if (unit == "km") {
    split_distance_miles <- split_distance / 1.60934  # convert km to miles
  } else if (unit == "mi") {
    split_distance_miles <- split_distance  # already in miles
  } else {
    stop("Invalid unit. Please specify 'km' or 'mi'.")
  }
  # Cumulative distance
  # cumulative_distance <- cumsum(distance_vector)
  # Identify split indices
  split_indices <- findInterval(distance, seq(0, max(distance), by = split_distance_miles))
  # Calculate time taken for each split
  split_times <- tapply(time_vector, split_indices, function(x) tail(x, 1) - head(x, 1))
  
  #Tabulate
  split_times_out <- convert_time(split_times);
  distance_num <- split_distance*1:length(split_times);
  distance_out <- paste(distance_num,unit);
    
  table_vis = data.frame(Split = distance_out, Time = split_times_out);
  table_num = data.frame(Split = distance_num, Time = split_times)
  # Create output
  result <- list(
    split_times = split_times,
    split_indices = split_indices,
    table_vis = table_vis,
    table_num = table_num
    
    
  )
  return(result)
}

# Convert to minutes and seconds
convert_time <- function(seconds) {
  return(ifelse(seconds >= 3600, 
                paste(seconds %/% 3600, "hrs", (seconds %% 3600) %/% 60, "min", seconds %% 60, "sec"), 
                paste((seconds %% 3600) %/% 60, "min", seconds %% 60, "sec")))
}

fastest_specified_interval <- function(data, interval_str) {
  # Extract the numeric value and unit from the input string
  interval_distance <- as.numeric(gsub("[^0-9.]", "", interval_str))
  distance_unit <- gsub("[0-9.]", "", interval_str)
  
  # Convert interval distance to the same unit as cumulative_distance if necessary
  if (distance_unit == "km") {
    interval_distance <- interval_distance / 1.60934  # Convert km to miles if cumulative_distance is in miles
  } else if (distance_unit == "mi") {
    interval_distance <- interval_distance  # No conversion needed for miles
  } else {
    stop("Unsupported distance unit. Use 'mi' or 'km'.")
  }
  
  # Initialize variables to track the minimum time and corresponding indices
  min_time <- Inf
  best_start <- NA
  best_end <- NA
  all_times <- NA
  
  cumulative_distance = fit_data$distance;
  time = fit_data$time;

  # Loop through cumulative distances to find the fastest specified interval
  for (start in 1:length(cumulative_distance)) {
    # Calculate the end point for the specified interval
    end_distance <- cumulative_distance[start] + interval_distance
    
    # Find the corresponding end index
    end_index <- which(cumulative_distance >= end_distance)
    
    if (length(end_index) > 0) {
      end_index <- end_index[1]  # Get the first occurrence
      if (end_index >= start) {
        # Calculate time for the current interval
        time_interval <- time[end_index] - ifelse(start == 1, 0, time[start - 1])
        all_times[start] <- time_interval
        # Update the minimum time if this interval's time is smaller
        if (time_interval < min_time) {
          min_time <- time_interval
          best_start <- start
          best_end <- end_index
        }
      }
    }
  }
  
  # Prepare result
  if (!is.na(best_start) && !is.na(best_end)) {
    result <- list(
      fastest_interval_distance = cumulative_distance[best_end] - cumulative_distance[best_start - 1],
      fastest_interval_time = min_time,
      all_times = all_times,
      df_split = data[best_start:best_end,]
    )
  } else {
    result <- list(
      fastest_interval_distance = NA,
      fastest_interval_time = NA,
      all_times = NA,
      df_split = NA
    )
  }
  
  return(result)
}

# Function to calculate split times for given distances
split_times_for_distances <- function(cumulative_distance, time, distance_strings) {
  # Convert distance strings to numeric values and units
  distances <- sapply(distance_strings, function(dist_str) {
    value <- as.numeric(gsub("[^0-9.]", "", dist_str))
    unit <- gsub("[0-9.]", "", dist_str)
    
    if (unit == "km") {
      return(value / 1.60934)  # Convert km to miles
    } else if (unit == "mi") {
      return(value)  # No conversion needed for miles
    } else {
      stop("Unsupported distance unit. Use 'mi' or 'km'.")
    }
  })
  
  # Initialize a vector to store split times
  split_times <- numeric(length(distances))
  
  # Loop through each distance and calculate the corresponding time
  for (i in seq_along(distances)) {
    target_distance <- distances[i]
    
    # Find the corresponding index for the target distance
    index <- which(cumulative_distance >= target_distance)
    
    if (length(index) > 0) {
      # Get the first occurrence
      index <- index[1]
      split_times[i] <- time[index] - time[1]
    } else {
      split_times[i] <- NA  # If distance is not reached
    }
  }
  
  table_out = data.frame(Distance = distance_strings);
  table_out$ElapsedTime=convert_time(split_times);
  table_out$OverallPace= convert_time(as.integer(split_times/distances));
  
  output<-list(
    splits = split_times,
    table = table_out
  )
  
  return(output)
}

################################################################################


csv_dir <- "/home/sivaprakasaman/Documents/Code/garmin_wrapped/Data/All_Fit_Files/fit_convert"
fig_dir <- "/home/sivaprakasaman/Documents/Code/garmin_wrapped/Figures"
run_date <- as.Date("2024-10-05")

cwd <- getwd();
setwd(csv_dir);

file_list <- list.files()
date_files <- NaN
dist_vect <- NaN
date_vect <- NaN
  
count <- 0;
count_fi <- 0;

for(i in file_list){
  count <- count +1;
  total_files <- length(file_list);
  print(paste('File: ', i, 'File', count, '/', total_files))
  
  fit_data <- read.csv(i)
  date <- as.Date(fit_data$datetime[1])
  dist <- max(fit_data$distance)
  #probably could write this more efficiently
  tryCatch(
    expr = {
      if(date==run_date){
        count_fi <- count_fi+1
        date_files[count_fi] = i;
        dist_vect[count_fi]= dist;
        date_vect[count_fi] = date;
        
        df_file <- data.frame(Date=date_vect, Distance=dist_vect, File=date_files);
      }
    },
    error = function(e){
      # (Optional)
      # Do this if an error is caught...
      print("ERROR: Unable to parse CSV")
    })
  
}

if(count_fi>1){
  print(head(df_file))
  s <- readline("Which file?")
  s <- as.numeric(s)
}

fit_data <- read.csv(df_file$File[s])
elevation_raw <- fit_data$elevation
time <- fit_data$time
distance <- fit_data$distance
# Example usage
distance_vector <- distance  # distances in miles
time_vector <- time  # times in seconds
split_distance_str <- "5km"  # specify split distance
# Call the function
splits_result <- calculate_splits_constant(distance_vector, time_vector, split_distance_str);
head(splits_result$table,30)

#path + map
coords <- data.frame(lon = fit_data$lon,lat = fit_data$lat, group=splits_result$split_indices)

seg_plot <- 
  st_as_sf(coords, coords = c("lon", "lat"), crs = "wgs84")  %>% 
  mutate(seg_end = lead(geometry)) %>% 
  rowwise() %>% 
  mutate(geometry = st_union(geometry, seg_end) %>% st_cast("LINESTRING")) %>% 
  ungroup() %>% 
  select(!starts_with("seg"))

pal <- colorNumeric(palette = "Dark2", domain = seg_plot$group) 

gradient_map <- leaflet(seg_plot) %>% 
  addTiles() %>% 
  addPolylines(color = ~pal(group), weight = 8, opacity = 0.5)

gradient_map
data = splits_result$table_num
data$group = 1:nrow(data)
data$timestring = splits_result$table_vis$Time

data$Time = data$Time/60;
ggplot(data,aes(x=Split)) + 
  geom_col(aes(y=Time, fill = as.factor(group))) +  
  scale_fill_manual(values =  pal(data$group))+ 
  geom_text(aes(label = timestring, y = Time),  vjust = -.2, color = "black", size = 3) +
  labs(title = "Split Paces", x = paste("Split (units of ",split_distance_str,")"), y = "Time (min)")+
  theme_pubclean()
  


fastest_mi = fastest_specified_interval(fit_data, "1mi") #returns in seconds!!
fastest_5k = fastest_specified_interval(fit_data, "5km") #returns in seconds!!
fastest_10k = fastest_specified_interval(fit_data, "10km") #returns in seconds!!
fastest_10mi = fastest_specified_interval(fit_data, "10mi");

fastest_dists = c(fastest_mi$fastest_interval_time,fastest_5k$fastest_interval_time,fastest_10k$fastest_interval_time, fastest_10mi$fastest_interval_time);
fastest_dists = convert_time(fastest_dists)

split_request = c('1mi','5km','5mi','10km','15km','10mi','20km','13.1mi')
all_splits = split_times_for_distances(distance_vector, time_vector,split_request)


