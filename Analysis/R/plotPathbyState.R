list.of.packages <- c('ggplot2', 'leaflet', 'ggmap','remotes','dplyr','purrr','PerformanceAnalytics','nloptr','lme4','lubridate','revgeo','maps')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages,library, character.only=TRUE)


csv_dir <- "/home/sivaprakasaman/Documents/Code/running_analytics/Data/All_Fit_Files/fit_convert"

cwd <- getwd();
setwd(csv_dir);

state_toSearch <- "pennsylvania"

file_list = list.files();
lat_list <- c();
lon_list <- c();

count <- 0;
for(i in file_list){
  count <- count +1;
  total_files <- length(file_list);
  print(paste('File: ', i, 'File', count, '/', total_files))
  
  fit_data <- read.csv(i)
  state <- map.where(database="state", fit_data$lon[1], fit_data$lat[1])
  
  tryCatch(
    expr = {
      if(state==state_toSearch){
        lat_list <- c(lat_list,fit_data$lat, NA)
        lon_list <- c(lon_list,fit_data$lon, NA)
      }    
    },
    error = function(e){ 
      # (Optional)
      # Do this if an error is caught...
      print("ERROR: Run probably not in United States")
    })
}


#plotting
coords <- cbind(lon_list,lat_list)

m <- coords %>%
  leaflet(  ) %>%
  addTiles() %>%
  addPolylines(color = 'black', weight=1.5)
m


setwd(cwd)
