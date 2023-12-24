list.of.packages <- c('ggplot2', 'dplyr','corrplot','PerformanceAnalytics','nloptr','lme4','lubridate', 'tidyr','ggpubr', 'rstatix')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages,library, character.only=TRUE)


data_dir <- "~/Documents/Code/running_analytics/Data/All_Fit_Files/all_meta"
cwd <- getwd();
setwd(data_dir);

dataset <- read.csv('full_meta_df.csv')
dataset$state <- as.factor(dataset$state)

data_dir <- data_dir <- "~/Documents/Code/running_analytics/Data/Activity Logs/";
setwd(data_dir);

activities <- read.csv("All_Activities_AS_to11_22_23.csv")
activities <- subset(activities, Activity.Type %in% c("Running","Trail Running","Track Running"))

dataset$date <- as.Date(dataset$date)
activities$Date <- as.Date(activities$Date)
activities$Total.Ascent <- as.numeric(activities$Total.Ascent)
activities$Total.Descent <- -as.numeric(activities$Total.Descent)

dataset$avg_ft_mi = dataset$total_Ascent2/dataset$distance;

start_date <- as.Date("2023-01-01")
end_date <- as.Date("2023-12-23")

dataset_year <- dataset[dataset$date >= start_date & dataset$date <= end_date,]
activities_year <- activities[activities$Date >= start_date & activities$Date <= end_date,]
  
p <- dataset_year %>%
  ggplot( aes(x=distance)) +
  geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.85) +
  ggtitle("Bin size = 1") +
  theme(
    plot.title = element_text(size=15)
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20))

p

p <- dataset_year %>%
  ggplot(aes(x=season)) +
  geom_bar(stat = "count") +
  labs(x = "Month")
p



dataset_elevation_stats <- select(dataset_year,c('total_Ascent','total_Descent','total_Ascent2','total_Descent2','total_Ascent3','total_Descent3','total_Ascent4','total_Descent4','total_Ascent5','total_Descent5'))
dataset_newnames <- c("Ascent_win10","Descent_win10","Ascent_win20","Descent_win20","Ascent_win40","Descent_win40","Ascent_win60","Descent_win60","Ascent_win80","Descent_win80")
names(dataset_elevation_stats) <- dataset_newnames

activity_elevation_stats <- select(activities_year,c('Total.Ascent','Total.Descent'))
activity_newnames <- c("Garmin_Ascent","Garmin_Descent")
names(activity_elevation_stats) <- activity_newnames

dataset_elevation_stats <- gather(dataset_elevation_stats, key = "Ascent_Window", value = "Estimate")
activity_elevation_stats <- gather(activity_elevation_stats, key = "Ascent_Window", value = "Estimate")

all_elev_est <- rbind(dataset_elevation_stats,activity_elevation_stats)
all_elev_est$Estimate <- as.numeric(all_elev_est$Estimate)
all_elev_est$Ascent_Window <- as.factor(all_elev_est$Ascent_Window)

all_elev_est <- na.omit(all_elev_est)
elevation_compare <- ggplot(all_elev_est, aes(x = reorder(Ascent_Window, -Estimate), y=Estimate)) +
  geom_jitter(size=1,width = 0.35, alpha=0.14) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Distribution of Multiple Columns",
       x = "Value",
       y = "Density",
       fill = "Variable")

elevation_compare

elevation_compare_loc <- ggplot(dataset, aes(x =  reorder(state, total_Ascent3), y= total_Ascent3)) +
  geom_jitter(size=1,width = 0.35, alpha=0.14) + 
  geom_boxplot(alpha = 0.7) +
  labs(title = "Distribution of Multiple Columns",
       x = "Value",
       y = "Density",
       fill = "Variable")

elevation_compare_loc

common_states_all <- dataset[dataset$state %in% c("west virginia", "indiana", "pennsylvania"),]
common_states_yr <- dataset_year[dataset_year$state %in% c("west virginia", "indiana", "pennsylvania"),]

stat.test <- common_states_all %>%
  t_test(avg_ft_mi ~ state)
  
stat.test_yr <- common_states_yr %>%
  t_test(avg_ft_mi ~ state)

stat.test <- stat.test %>% add_xy_position(x = "state")
stat.test_yr <- stat.test_yr %>% add_xy_position(x = "state")

col_array <- c("#990000", "#FFB81C", "#002855");
elevation_compare_runavg <- ggplot(common_states_all, aes(x = state, y = avg_ft_mi, fill = state)) +
  geom_jitter(size=1,width = .25, alpha=0.1) + 
  geom_violin(alpha = 0.3, trim = FALSE) +
  geom_boxplot(width = .2, alpha = .7) +
  scale_fill_manual(values=col_array)+
  scale_color_manual(values=col_array)+
  labs(title = "Hilliness of Runs Across States",
       x = "State",
       y = "Average Elevation Gain per Mile (ft/mi)") +
  theme(legend.position = "none")+
  ylim(0,450)+
  stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0.01, y.position = c(420, 450, 420),bracket.shorten = 0.1, inherit.aes=FALSE)
  # 

elevation_compare_runavg

hr_season <- ggplot(dataset, aes(x = season, y= mean_heart_rate)) +
  geom_jitter(size=1,width = 0.35, alpha=0.14) + 
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun.y = median, fun.max = length,
               geom = "text", aes(label = paste('N =',..ymax..)), vjust = -.8)+
  labs(title = "Distribution of Multiple Columns",
       x = "Season",
       y = "Avg. Heart Rate")

hr_season

hr_season2 <- ggplot() + 
  geom_boxplot(data = dataset, aes(x = season, y = mean_heart_rate, fill = "Average")) +
  geom_boxplot(data = dataset_year, aes(x = season, y = mean_heart_rate, fill = "This Year")) +labs(title = "Side-by-Side Box Plots",
                                                             x = "Group",
                                                             y = "Value",
                                                             fill = "Data Frame") +
  scale_fill_manual(values = c("Average" = "blue", "This Year" = "purple"))

hr_season2

setwd(cwd)
