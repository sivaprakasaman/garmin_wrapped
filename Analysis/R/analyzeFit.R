data_dir <- "/home/sivaprakasaman/Documents/Code/running_analytics/Data/testSet";
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
  
  sport_msg = getMessagesByType(fit,'sport');
  activity_type = sport_msg[[2]];
    
  Distance <- fit_allrecords$distance
  Distance <- Distance/1609.34; #to mi
  distance_shift <- c(0,Distance)
  distance_shift <- distance_shift[1:length(distance_shift)-1];
    
  time_absolute <- as.POSIXct(fit_allrecords$timestamp);
  t_vect <- as.numeric(time_absolute);
  Time <- t_vect-min(t_vect);
  time_shift <- c(0,Time);
  time_shift <- time_shift[1:length(time_shift)-1];
  
  dx <- Distance-distance_shift; #delta position
  dt <- Time-time_shift;
  
  #in mi/s
  Pace <- dx/dt;
  Pace <- 1/(Pace*60);

  #converting cadence (only needed for non-biking)
  Cadence = (fit_allrecords$cadence+fit_allrecords$fractional_cadence)*2;
  
  #converting altitude...actually enhanced_altitude is just in meters?? altitude may be something different??
  
  #if barometer data
  if("enhanced_altitude" %in% colnames(fit_allrecords)){
    Elevation = fit_allrecords$enhanced_altitude
  }else if ("altitude" %in% colnames(fit_allrecords)){
    Elevation = fit_allrecords$altitude
  }else{
    Elevation = matrix(NA, ncol = 1, nrow = nrow(fit_allrecords)) 
  }
  
  Elevation = Elevation*3.28084; #to feet
    
  #other data (some of which might not be present)
  data_heads = c("heart_rate","step_length","stance_time_balance","vertical_ratio","stance_time","power")
  other_data = search_and_extract_headers(fit_allrecords,data_heads);
  
  # Create a data frame
  data <- data.frame(distance = Distance, pace = Pace, cadence = Cadence, elevation = Elevation, lat = fit_allrecords$position_lat, lon = fit_allrecords$position_long)
  data <- cbind(data, other_data)
    
  # Z-score data, and reject any points > +\- 3SDs above mean (assume measurement noise)
  data <- remove_outliers_df(data)
  data$time <- Time;
  data = relocate(data,'time',1);

  elev_gainloss = calculate_elevation_gain_smoothed(Elevation,window_size = 40);
  meta_data <- data.frame(Activity = activity_type, total_Ascent = elev_gainloss[[1]], total_Ascent = elev_gainloss[[2]]);
  
  out = list(data,meta_data);
  
  return(out)
  
}

# Function to read either FIT or TCX file
read_fitness_data <- function(file_path) {
  file_extension <- tools::file_ext(file_path)
  if (tolower(file_extension) == "tcx") {
    return(read_tcx(file_path))
  } else if (tolower(file_extension) == "fit") {
    return(read_fit(file_path))
  } else if (tolower(file_extension) == "txt") {
    return(read_tcx(file_path))
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


search_and_extract_headers <- function(df, headers_to_search) {
  # Create a new dataframe with NAs
  new_df <- data.frame(matrix(NA, ncol = length(headers_to_search), nrow = nrow(df)))
  colnames(new_df) <- headers_to_search
  
  # Loop through the specified headers and fill in the new dataframe
  for (header in headers_to_search) {
    if (header %in% colnames(df)) {
      new_df[, header] <- df[, header]
    }
  }
  
  return(new_df)
}

# Example usage
file_path <- "Marshall_Half_Marathon_2012.tcx"  # Replace with the actual file path

file_path <- "Zionsville_HM_23.fit"  # Replace with the actual file path
file_path <- "PGH_10M_2016.fit";
file_path <- "gym_workout.fit";
# file_path <- "notTCX2.txt";

# Read FIT or TCX file
tryCatch(
  expr = {
    fitness_data <- read_fitness_data(file_path);
  },
  error = function(e){ 
    # (Optional)
    # Do this if an error is caught...
    print("ERROR: Can't parse file.")
  },
  finally = {
    print(paste(file_path,' Processing Complete'))
  }
)

# Print the extracted data
print("Fitness Data:")
print(fitness_data)

fit_df = fitness_data[[1]];
fit_meta = fitness_data[[2]];

# Plot map
ggplot(fit_df, aes(x = lon, y = lat)) +
  coord_quickmap() +
  geom_point(aes(colour = fit_df$distance))

coords <- cbind(fit_df$lon,fit_df$lat)

m <- coords %>% 
  leaflet(  ) %>%
  addTiles() %>%
  addPolylines(color = 'red')
m

setwd(cwd)

