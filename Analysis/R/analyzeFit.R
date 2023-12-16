data_dir <- "/home/sivaprakasaman/Documents/Code/running_analytics/Data/Races/HM";
cwd <- getwd();
setwd(data_dir);



## Installing Dependencies & Importing Libraries
list.of.packages <- c('ggplot2', 'leaflet', 'ggmap','remotes','dplyr','purrr','PerformanceAnalytics','nloptr','lme4','lubridate')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#figure out where to install these
library("trackeR")
library("FITfileR")
lapply(list.of.packages,library, character.only=TRUE)


# if(!requireNamespace("remotes")) {
#   install.packages("remotes")in
# }
# remotes::install_github("grimbough/FITfileR")
# devtools::install_github("trackerproject/trackeR")

# Function to remove outliers from a numeric vector using Z-score
remove_outliers <- function(x, threshold = 3) {
  z_scores <- abs(scale(x))
  outliers <- which(z_scores > threshold)
  
  for (i in outliers) {
    # Find the indices of the 5 nearest non-outlier points
    nearest_non_outliers <- order(abs(outliers - i))[1:5]
    
    # Interpolate the outlier value based on the average of the nearest non-outlier points
    x[i] <- mean(x[nearest_non_outliers], na.rm = TRUE)
  }
  
  return(x)
}

# Function to remove outliers from all numeric columns in a dataframe
remove_outliers_df <- function(df, threshold = 3) {
  df %>% 
    mutate(across(where(is.numeric), ~remove_outliers(.)))
}

# Function to extract information from TCX file
read_tcx <- function(tcx_file) {
  tcx <- readTCX(tcx_file, distanceunit = 'm')
  
  # Extract relevant information
  distance <- tcx$distance
  distance <- distance/1609.34; #to mi
  distance_shift <- c(0,distance)
  distance_shift <- distance_shift[1:length(distance_shift)-1];
  
  time_absolute <- as.POSIXct(tcx$time);
  t_vect <- as.numeric(time_absolute);
  time <- t_vect-min(t_vect);
  time_shift <- c(0,time);
  time_shift <- time_shift[1:length(time_shift)-1];
  
  dx <- distance-distance_shift; #delta position
  dt <- time-time_shift;
  
  #in mi/s
  pace <- dx/dt;
  pace <- 1/(pace*60);
  
  # Create a data frame
  data <- data.frame(Time = time, Distance = distance, Pace = pace, Lat = tcx$latitude, Lon = tcx$longitude)
  
  return(data)
}

# Function to extract information from FIT file using FITFileR
read_fit <- function(fit_file) {
  fit <- readFitFile(fit_file)
  
  # Extract relevant information using records
  fit_allrecords <- records(fit) %>% 
    bind_rows() %>% 
    arrange(timestamp) 
  
  distance <- fit_allrecords$distance
  distance <- distance/1609.34; #to mi
  distance_shift <- c(0,distance)
  distance_shift <- distance_shift[1:length(distance_shift)-1];
    
  time_absolute <- as.POSIXct(fit_allrecords$timestamp);
  t_vect <- as.numeric(time_absolute);
  time <- t_vect-min(t_vect);
  time_shift <- c(0,time);
  time_shift <- time_shift[1:length(time_shift)-1];
  
  dx <- distance-distance_shift; #delta position
  dt <- time-time_shift;
  
  #in mi/s
  pace <- dx/dt;
  pace <- 1/(pace*60);

  #converting cadence (only needed for non-biking)
  cadence = (fit_allrecords$cadence+fit_allrecords$fractional_cadence)*2;
  
  #converting altitude...actually enhanced_altitude is just in meters?? altitude may be something different??
  elevation = fit_allrecords$enhanced_altitude*3.28084; #to feet
  
  #other data (some of which might not be present)
  data_heads = c("heart_rate","cadence","fractional_cadence","step_length","stance_time_balance","vertical_ratio","stance_time","power")
  
  # Create a data frame
  data <- data.frame(Time = time, Distance = distance, Pace = pace, Cadence = cadence, Elevation = elevation, Power = fit_allrecords$power,Lat = fit_allrecords$position_lat, Lon = fit_allrecords$position_long)

  # Z-score data, and reject any points > +\- 3SDs above mean (assume measurement noise)
  data <- remove_outliers_df(data)
  
  activity_type = fit_allrecords$activity_type[1];
  
  meta_data <- data.frame(Activity = activity_type, total_Ascent = ascent, total_Descent = descent);
  
  return(out)
  
}

# Function to read either FIT or TCX file
read_fitness_data <- function(file_path) {
  file_extension <- tools::file_ext(file_path)
  
  if (tolower(file_extension) == "tcx") {
    return(read_tcx(file_path))
  } else if (tolower(file_extension) == "fit") {
    return(read_fit(file_path))
  } else {
    stop("Unsupported file type. Only TCX and FIT files are supported.")
  }
}

calc_AscentDescent <- function(elevation){
  
  elev_shift <- c(0,elevation);
  elev_shift <- elev_shift[1:length(elev_shift)-1];
  d_elevation <- elevation-elev_shift;
  d_elevation <- d_elevation[2:length(d_elevation)]
  ascent = sum(d_elevation[which(d_elevation >= 0.2)]);
  descent = sum(d_elevation[which(d_elevation <= -0.2)]);
  
  return(list(ascent,descent,d_elevation));
  
}


calculate_elevation_gain_smoothed <- function(elevation_plot, window_size = 40) {
  # Ensure elevation_plot is a numeric vector
  if (!is.numeric(elevation_plot)) {
    stop("Input must be a numeric vector.")
  }
  
  # Apply a simple moving average (rolling mean) for smoothing
  smoothed_elevation <- zoo::rollmean(elevation_plot, k = window_size, fill = NA)
  
  # Calculate differences between consecutive smoothed elevation points
  smoothed_diff <- diff(smoothed_elevation)
  
  # Sum positive differences (elevation gain)
  elevation_gain <- sum(smoothed_diff[smoothed_diff > 0], na.rm = TRUE)
  elevation_loss <- sum(smoothed_diff[smoothed_diff < 0], na.rm = TRUE)
  
  return(list(elevation_gain,elevation_loss, smoothed_elevation))
}

# Example usage
file_path <- "Marshall_Half_Marathon_2012.tcx"  # Replace with the actual file path
file_path <- "Zionsville_HM_23.fit"  # Replace with the actual file path

# Read FIT or TCX file
fitness_data <- read_fitness_data(file_path)

# Print the extracted data
print("Fitness Data:")
print(fitness_data)

# Plot map
ggplot(fitness_data, aes(x = Lon, y = Lat)) +
  coord_quickmap() +
  geom_point(aes(colour = fitness_data$Distance))

coords <- cbind(fitness_data$Lon,fitness_data$Lat)

m <- coords %>% 
  leaflet(  ) %>%
  addTiles() %>%
  addPolylines(color = 'red')

m

setwd(cwd)

