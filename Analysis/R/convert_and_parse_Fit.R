#Author: Andrew Sivaprakasam
#Last Updated: January 2024
#Description: This code converts all the running activity fit files in a specified folder to 
#csv files, saving summary data into a separated file for analysis in process_MetaData.

############# Prerequisite Package Loading and Installing ######################
## Installing Dependencies & Importing Libraries
list.of.packages <- c('ggplot2','remotes','dplyr','purrr','PerformanceAnalytics','nloptr','lubridate','revgeo','maps')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#Where you can install FITfileR and trackeR
# if(!requireNamespace("remotes")) {
#   install.packages("remotes")in
# }
# remotes::install_github("grimbough/FITfileR")
# devtools::install_github("trackerproject/trackeR")

#figure out where to install these
library("trackeR")
library("FITfileR")
lapply(list.of.packages,library, character.only=TRUE)

############################## HELPER FUNCTIONS ################################
# Function to remove outliers from a numeric vector using Z-score
remove_outliers <- function(x, threshold = 3) {
  x[is.infinite(x)] <- 0
  z_scores <- abs(scale(x))
  outliers <- which(z_scores > threshold);
  non_outliers <- which(z_scores < threshold);
  
  for (i in outliers) {
    # Find the indices of the 5 nearest non-outlier points
    nearest_non_outliers <- order(abs(non_outliers - i))[1:5];
    
    # Interpolate the outlier value based on the average of the nearest non-outlier points
    x[i] <- mean(x[non_outliers[nearest_non_outliers]], na.rm = TRUE)
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
  
  activity_type = 'running'; #Assume for old non-fit data always running
  
  # Extract relevant information
  Distance <- tcx$distance
  Distance <- Distance/1609.34; #to mi
  distance_shift <- c(0,Distance)
  distance_shift <- distance_shift[1:length(distance_shift)-1];
  datetime = tcx$time[1];
  season_date = get_season(datetime);
  
  time_absolute <- as.POSIXct(tcx$time);
  t_vect <- as.numeric(time_absolute);
  Time <- t_vect-min(t_vect);
  time_shift <- c(0,Time);
  time_shift <- time_shift[1:length(time_shift)-1];
  
  dx <- Distance-distance_shift; #delta position
  dt <- Time-time_shift;
  
  #in mi/s
  Pace <- dx/dt;
  Pace <- 1/(Pace*60);
  
  #Elevation
  Elevation = tcx$altitude*3.28084; #to feet
  
  # Create a data frame
  data <- data.frame(pace = Pace, lat = tcx$latitude, lon = tcx$longitude, elevation = Elevation)
  
  # Rearrange, remove outliers
  data <- remove_outliers_df(data)
  data_means <- calculate_column_means(data)
  data$time <- Time;
  data$datetime<-tcx$time
  data$distance <- Distance;
  data = relocate(data,'time',1);
  data = relocate(data,'distance',1);
  
  # Summary data, assume activities with gpx or text data are running
  
  elev_gainloss = calculate_elevation_gain_smoothed(Elevation,window_size = 10);
  elev_gainloss2 = calculate_elevation_gain_smoothed(Elevation,window_size = 20);
  elev_gainloss3 = calculate_elevation_gain_smoothed(Elevation,window_size = 40);
  elev_gainloss4 = calculate_elevation_gain_smoothed(Elevation,window_size = 60);
  elev_gainloss5 = calculate_elevation_gain_smoothed(Elevation,window_size = 80);
  
  meta_data <- data.frame(activity = activity_type, date = as.character(datetime), season = season_date, distance = max(Distance, na.rm = TRUE), time = max(Time)/60, total_Ascent = elev_gainloss[[1]], total_Descent = elev_gainloss[[2]]);
  meta_data <- cbind(meta_data, data_means)
  
  #Being lazy here...just trying to figure out a good smoothing parameter for later
  meta_data$total_Ascent2 <- elev_gainloss2[[1]]
  meta_data$total_Descent2 <- elev_gainloss2[[2]]
  meta_data$total_Ascent3 <- elev_gainloss3[[1]]
  meta_data$total_Descent3 <- elev_gainloss3[[2]]
  meta_data$total_Ascent4 <- elev_gainloss4[[1]]
  meta_data$total_Descent4 <- elev_gainloss4[[2]]
  meta_data$total_Ascent5 <- elev_gainloss5[[1]]
  meta_data$total_Descent5 <- elev_gainloss5[[2]]
  
  meta_data$state <- map.where(database="state", tcx$longitude[1], tcx$latitude[1])
  meta_data$county <- map.where(database="county", tcx$longitude[1], tcx$latitude[1])
  
  out = list(data,meta_data);
  
  return(out)
}

# Function to extract information from FIT file using FITFileR
read_fit <- function(fit_file) {
  fit <- readFitFile(fit_file)
  
  tryCatch(
    expr = {
      sport_msg = getMessagesByType(fit,'sport');
    },
    error = function(e){ 
      # (Optional)
      # Do this if an error is caught...
      return(NULL)
      print("ERROR: Can't parse file. Returning...")
    }
  )
  
  # Extract relevant information using records
  fit_allrecords <- records(fit) %>% 
    bind_rows() %>% 
    arrange(timestamp) 
  
  activity_type = sport_msg[[2]];
  
  if(tolower(activity_type) != "running"){
    print(paste('Activity Type: ', activity_type,'-- Skipping'))
    return(NULL)
  }
  
  
  Distance <- fit_allrecords$distance
  Distance <- Distance/1609.34; #to mi
  distance_shift <- c(0,Distance)
  distance_shift <- distance_shift[1:length(distance_shift)-1];
  datetime = fit_allrecords$timestamp[1];
  season_date = get_season(datetime);
  
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
  data_heads = c("heart_rate","step_length","stance_time_balance","vertical_ratio","stance_time","vertical_oscillation","power")
  other_data = search_and_extract_headers(fit_allrecords,data_heads);
  
  # Create a data frame
  data <- data.frame(pace = Pace, cadence = Cadence, elevation = Elevation, lat = fit_allrecords$position_lat, lon = fit_allrecords$position_long)
  data <- cbind(data, other_data)
  
  # Z-score data, and reject any points > +\- 3SDs above mean (assume measurement noise)
  data <- remove_outliers_df(data)
  data_means <- calculate_column_means(data)
  data$time <- Time;
  data$datetime<-fit_allrecords$timestamp
  data$distance <- Distance;
  data = relocate(data,'time',1);
  data = relocate(data,'distance',1);
  
  elev_gainloss = calculate_elevation_gain_smoothed(Elevation,window_size = 10);
  elev_gainloss2 = calculate_elevation_gain_smoothed(Elevation,window_size = 20);
  elev_gainloss3 = calculate_elevation_gain_smoothed(Elevation,window_size = 40);
  elev_gainloss4 = calculate_elevation_gain_smoothed(Elevation,window_size = 60);
  elev_gainloss5 = calculate_elevation_gain_smoothed(Elevation,window_size = 80);
  
  meta_data <- data.frame(activity = activity_type, date = as.character(datetime), season = season_date, distance = max(Distance, na.rm = TRUE), time = max(Time)/60, total_Ascent = elev_gainloss[[1]], total_Descent = elev_gainloss[[2]]);
  meta_data <- cbind(meta_data, data_means)
  
  #Being lazy here...just trying to figure out a good smoothing parameter for later
  meta_data$total_Ascent2 <- elev_gainloss2[[1]]
  meta_data$total_Descent2 <- elev_gainloss2[[2]]
  meta_data$total_Ascent3 <- elev_gainloss3[[1]]
  meta_data$total_Descent3 <- elev_gainloss3[[2]]
  meta_data$total_Ascent4 <- elev_gainloss4[[1]]
  meta_data$total_Descent4 <- elev_gainloss4[[2]]
  meta_data$total_Ascent5 <- elev_gainloss5[[1]]
  meta_data$total_Descent5 <- elev_gainloss5[[2]]
  
  meta_data$state <- map.where(database="state", fit_allrecords$position_long[1], fit_allrecords$position_lat[1])
  meta_data$county <- map.where(database="county", fit_allrecords$position_long[1], fit_allrecords$position_lat[1])
  
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

calculate_column_means <- function(input_df) {
  means <- colMeans(input_df, na.rm = TRUE)
  means_df <- data.frame(t(means))
  
  # Modify column names to "mean_previous_column"
  colnames(means_df) <- paste("mean_", colnames(input_df), sep = "")
  
  return(means_df)
}

get_season <- function(date) {
  # Extract month and day from the date
  month_day <- format(date, "%m-%d")
  
  # Determine the season based on the month and day
  if (month_day >= "03-21" && month_day <= "06-20") {
    return("Spring")
  } else if (month_day >= "06-21" && month_day <= "09-22") {
    return("Summer")
  } else if (month_day >= "09-23" && month_day <= "12-20") {
    return("Fall")
  } else {
    return("Winter")
  }
}

############################## CODE ############################################

#Here are the directories you will need to change!!!!!
data_dir <- "~/Documents/Code/garmin_wrapped/Data/All_Fit_Files/all_fit"; #Folder with all your fit files
out_dir <- "~/Documents/Code/garmin_wrapped/Data/All_Fit_Files/fit_convert"; #Where you want the converted csv files to live
meta_dir <- "~/Documents/Code/garmin_wrapped/Data/All_Fit_Files/all_meta"; #Where the metadata (summary files) will be saved


dir.create(file.path(out_dir), showWarnings = FALSE)
dir.create(file.path(meta_dir), showWarnings = FALSE)
setwd(data_dir)

cwd <- getwd();
setwd(data_dir);

start_time <- Sys.time()

append_new = FALSE; #TRUE if you have an existing meta_df csv you just want to add to

file_list = list.files();

if(append_new){
  setwd(meta_dir)
  meta_df_all <- read.csv('full_meta_df.csv');
  setwd(data_dir)
} else{
  meta_df_all <- data.frame(); #initialize empty
}

count <- 0;
for(i in file_list){
  count <- count +1;
  total_files <- length(file_list);
  print(paste('File: ', i, 'File', count, '/', total_files))
  fitness_data <- NULL;
  tryCatch(
    expr = {
      fitness_data <- read_fitness_data(i);
    },
    error = function(e){ 
      # (Optional)
      # Do this if an error is caught...
      print("ERROR: Can't parse file.")
    },
    finally = {
      print(paste(i,' Processing Attempted'))
    }
  )
  
  if(!is.null(fitness_data)){
    #appending meta_data
    meta_df_all <- dplyr::bind_rows(meta_df_all,fitness_data[[2]]);
    setwd(out_dir);
    fname <- paste0(tools::file_path_sans_ext(i),'.csv')
    write.csv(fitness_data[[1]],fname, row.names = FALSE);
    setwd(data_dir);
  }
  
  setwd(meta_dir)
  write.csv(meta_df_all,'full_meta_df.csv', row.names = FALSE);
  setwd(data_dir);
}

end_time <- Sys.time()
time_fin <- end_time-start_time
print(time_fin)
setwd(cwd)

