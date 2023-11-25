data_dir <- "/home/sivaprakasaman/Documents/Code/running_analytics/Data/Races/HM";
cwd <- getwd();
setwd(data_dir);



## Installing Dependencies & Importing Libraries
list.of.packages <- c('ggplot2', 'leaflet', 'ggmap','remotes','dplyr','corrplot','PerformanceAnalytics','nloptr','lme4','lubridate')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("trackeR")
library("FITfileR")
lapply(list.of.packages,library, character.only=TRUE)


# if(!requireNamespace("remotes")) {
#   install.packages("remotes")in
# }
# remotes::install_github("grimbough/FITfileR")
# devtools::install_github("trackerproject/trackeR")

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

  # Create a data frame
  data <- data.frame(Time = time, Distance = distance, Pace = pace, Lat = fit_allrecords$position_lat, Lon = fit_allrecords$position_long)
  return(data)
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

