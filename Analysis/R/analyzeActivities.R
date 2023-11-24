
data_dir <- "/home/sivaprakasaman/Documents/Code/running_analytics/Data/Activity Logs/";
cwd <- getwd();

setwd(data_dir);

## Installing Dependencies & Importing Libraries
list.of.packages <- c('ggplot2', 'dplyr','corrplot','PerformanceAnalytics','nloptr','lme4','lubridate')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages,library, character.only=TRUE)

activities <- read.csv("All_Activities_AS_to11_22_23.csv")

running_only <- subset(activities, Activity.Type %in% c("Running","Trail Running"))
track_only <- subset(activities, Activity.Type %in% c("Track Running"));
track_only$Distance = as.numeric(as.numeric(gsub(",", "", track_only$Distance)))/1609.344;

# running_only <- data.frame(running_only);
# track_only <-

running_only <- rbind(running_only,track_only)
running_only$Distance = as.numeric(running_only$Distance);
running_only$Time = as.numeric(period_to_seconds(hms(running_only$Time)));
running_only$Avg.Pace = as.numeric(period_to_seconds(ms(running_only$Avg.Pace)));

running_only$Time = running_only$Time/60;
running_only$Avg.Pace = running_only$Avg.Pace/60;

running_only$Avg.HR = as.numeric(running_only$Avg.HR);


hr_present <- subset(running_only, Avg.HR>0);
plot_dist <- ggplot(hr_present, aes(x=Avg.Pace,y=Avg.HR))
plot_dist <- plot_dist+geom_point(alpha=0.5, aes(colour = Avg.HR))
print(plot_dist)


setwd(cwd);
