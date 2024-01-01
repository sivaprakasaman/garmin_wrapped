#Author: Andrew Sivaprakasam
#Last Updated: January 2024
#Description: This code was just written to start analyzing the data that can be exported 
#directly from Garmin Connect (a tedious scrolling process to get a limited csv).
#I wouldn't spend much time in this script :)


data_dir <- "~/Documents/Code/garmin_wrapped/DEMO_Data/Activity_Log_fromGarmin/";
cwd <- getwd();

setwd(data_dir);

## Installing Dependencies & Importing Libraries
list.of.packages <- c('ggplot2', 'dplyr','corrplot','PerformanceAnalytics','nloptr','lme4','lubridate')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages,library, character.only=TRUE)

activities <- read.csv("Activities_to_12_24_23.csv")

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
running_only$Total.Ascent = as.numeric(running_only$Total.Ascent);
running_only$Total.Descent = as.numeric(running_only$Total.Descent);

hr_present <- subset(running_only, Avg.HR>0);
plot_dist <- ggplot(hr_present, aes(x=Avg.Pace,y=Avg.HR))
plot_dist <- plot_dist+geom_point(alpha=0.5, aes(colour = Avg.HR))
print(plot_dist)

line<-par(lwd=2)
h_dist = hist(running_only$Distance, xlab = "Distance (Mi)",ylab = "Count", freq = TRUE,col ="darkgoldenrod",main = paste("Distribution of Distance Run | N =", toString(length(running_only$Distance)), "Runs"))
text(h_dist$mids,h_dist$counts,labels=h_diste$counts, adj=c(0.5, -0.5), cex = 1.5)
# axis(side=1, at=c(0,18,35,60,90), labels=c(0,18,35,60,90))


p <- running_only %>%
  ggplot( aes(x=Distance)) +
  geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.85) +
  ggtitle("Bin size = 1") +
  theme(
    plot.title = element_text(size=15)
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20))

p

p2 <- running_only %>%
  ggplot( aes(x=Distance)) +
  geom_histogram(  breaks = c(0,1,3,6,10,15,26), fill="#69b3a2", color="#e9ecef", alpha=0.85) +
  ggtitle("Bin size = 1") +
  theme(
    plot.title = element_text(size=15)
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20))

p3 <- running_only %>%
  ggplot(aes(x = 1, y=Total.Descent)) +
  geom_jitter(size=1,width = 0.35, alpha=0.14) + 
  geom_boxplot(alpha = 0.7) +
  labs(title = "Distribution of Multiple Columns",
       x = "Window",
       y = "Elevation Gain/Loss Estimate",
       fill = "Variable")


p3

setwd(cwd)

