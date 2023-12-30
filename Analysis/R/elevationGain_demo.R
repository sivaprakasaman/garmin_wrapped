
#Assumes that you've already run the analyzeFit.R script to convert all running .fits 
#to CSV

#Elevation Gain computing function (same function used to compute the metadata summary metrics)
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

####################################################################################

#this can be used if you know the date of a particular run you want to compare
#or, you can just specify a filename manually

# Enable this once CSV has datetime as well
# run_date <- as.Date("2023-01-01")
# 
# count <- 0;
# for(i in file_list){
#   count <- count +1;
#   total_files <- length(file_list);
#   print(paste('File: ', i, 'File', count, '/', total_files))
#   
#   fit_data <- read.csv(i)
#   date <- fit_data$
#   
#   #probably could write this more efficiently
#   tryCatch(
#     expr = {
#       if(show_all){
#         lat_list <- c(lat_list,fit_data$lat, NA)
#         lon_list <- c(lon_list,fit_data$lon, NA)
#       }else if(!show_all && state==state_toSearch){
#         lat_list <- c(lat_list,fit_data$lat, NA)
#         lon_list <- c(lon_list,fit_data$lon, NA)
#       }
#     },
#     error = function(e){ 
#       # (Optional)
#       # Do this if an error is caught...
#       print("ERROR: Run probably not in United States")
#     })
# }
csv_dir <- "/home/sivaprakasaman/Documents/Code/running_analytics/Data/All_Fit_Files/fit_convert"
csv_to_check <- "sivaprakasaman_12633906948.csv"

cwd <- getwd();
setwd(csv_dir);

data <- read.csv(csv_to_check)
elevation_raw <- data$elevation

#no smoothing window
ascent_descent <- calculate_elevation_gain_smoothed(elevation_raw,window_size = 1)
#10s smoothing window
ascent_descent10 <- calculate_elevation_gain_smoothed(elevation_raw,window_size = 10)
#40s smoothing window
ascent_descent40 <- calculate_elevation_gain_smoothed(elevation_raw,window_size = 40)
#80s smoothing window
ascent_descent80 <- calculate_elevation_gain_smoothed(elevation_raw,window_size = 80)

#smooth_elevation
all_smooth <- data.frame(no_win = ascent_descent[[3]], win10 = ascent_descent10[[3]], win40 = ascent_descent40[[3]], win80 = ascent_descent80[[3]])
all_smooth$time <- data$time

#calculated ascent
ascent_all <- data.frame(no_win = ascent_descent[[1]], win10 = ascent_descent10[[1]], win40 = ascent_descent40[[1]], win80 = ascent_descent80[[1]])
no_win_str = paste0(sprintf('%.f',ascent_descent[[1]]), ' ft');
win10_str = paste0(sprintf('%.f',ascent_descent10[[1]]), ' ft');
win40_str = paste0(sprintf('%.f',ascent_descent40[[1]]), ' ft');
win80_str = paste0(sprintf('%.f',ascent_descent80[[1]]), ' ft');




smoothed_elevation <- ggplot(all_smooth, aes(x =time/60))+
  geom_line(aes(y=no_win, color = "None"), size=1)+
  geom_text(aes(x=90, y= 212, label = 'Est. Elevation Gain:'), color = 'black')+
  geom_text(aes(x=90,y=200, label = no_win_str, color='None'), show.legend=FALSE)+
  geom_text(aes(x=90,y=185, label = win10_str, color='10s'), show.legend=FALSE)+
  geom_text(aes(x=90,y=170, label = win40_str, color='40s'), show.legend=FALSE)+
  geom_text(aes(x=90,y=155, label = win80_str, color='80s'), show.legend=FALSE)+
  geom_line(aes(y=win10, color = "10s"), size=1)+
  geom_line(aes(y=win40, color = "40s"), size=1)+
  geom_line(aes(y=win80, color = "80s"), size=1)+
  scale_color_manual(name = "Window Size", values = c("None" = "gray80", "10s" = "gray70", "40s" = "cornflowerblue", "80s" = "orchid"),breaks = c("None", "10s",  "40s","80s"))+
  labs(title = "Window Size Comparison",
     x = "Time (min)",
     y = "Elevation (ft)")
smoothed_elevation


setwd(cwd)
