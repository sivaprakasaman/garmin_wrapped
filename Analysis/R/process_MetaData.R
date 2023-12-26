list.of.packages <- c('ggplot2', 'dplyr','corrplot','PerformanceAnalytics','nloptr','lme4','lubridate', 'tidyr','ggpubr', 'rstatix','RColorBrewer')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages,library, character.only=TRUE)


data_dir <- "~/Documents/Code/running_analytics/Data/All_Fit_Files/all_meta"
cwd <- getwd();
setwd(data_dir);

dataset <- read.csv('full_meta_df.csv')
# dataset$state <- as.factor(dataset$state)

data_dir <- data_dir <- "~/Documents/Code/running_analytics/Data/Activity Logs/";
setwd(data_dir);

activities <- read.csv("Activities_to_12_24_23.csv")
activities <- subset(activities, Activity.Type %in% c("Running","Trail Running","Track Running"))

dataset$date <- as.Date(dataset$date)
activities$Date <- as.Date(activities$Date)
activities$Total.Ascent <- as.numeric(activities$Total.Ascent)
activities$Total.Descent <- -as.numeric(activities$Total.Descent)

dataset$avg_ft_mi = dataset$total_Ascent2/dataset$distance;
dataset$avg_ft_mi = as.numeric(dataset$avg_ft_mi);

start_date <- as.Date("2023-01-01")
end_date <- as.Date("2023-12-24")

dataset_year <- dataset[dataset$date >= start_date & dataset$date <= end_date,]
activities_year <- activities[activities$Date >= start_date & activities$Date <= end_date,]
  
# p <- dataset_year %>%
#   ggplot( aes(x=distance)) +
#   geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.85) +
#   ggtitle("Bin size = 1") +
#   theme(
#     plot.title = element_text(size=15)
#   ) +
#   scale_x_continuous(breaks = scales::pretty_breaks(n = 20))
# 
# p

dataset$year <- format(dataset$date, "%Y")
year_plot_Data <- aggregate(distance ~ year, data = dataset, sum)

colourCount = nrow(year_plot_Data)
getPalette = colorRampPalette(brewer.pal(9, "PuBu"))

year_plot <- ggplot(year_plot_Data, aes(x = year, y = distance, fill = year)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.0f", distance), y = distance),  vjust = -.2, color = "black", size = 3.5) +
  labs(title = "Distance Run Each Year",
       x = "Year",
       y = "Total Distance (Mi)") +
  theme_pubclean()+
  scale_fill_manual(values = getPalette(colourCount))+
  theme(panel.background = element_rect(fill = "gray94"))+
  theme(legend.position = "none")

year_plot


clr_dist = "#390273";
p <-ggplot()+
  geom_histogram(data=dataset,aes(x=distance, fill="All Time", alpha="All Time"), binwidth=2, color="#e9ecef") +
  stat_bin(data=dataset,aes(x = distance, label=..count.., color="All Time", alpha="All Time"), binwidth = 2, geom="text", vjust=-1.5, show.legend = FALSE) +
  stat_bin(data=dataset_year,aes(x = distance, label=..count..,color="This Year", alpha="This Year"), binwidth = 2, geom="text", vjust=-.5, show.legend = FALSE) +
  geom_histogram(data=dataset_year,aes(x=distance, fill="This Year", alpha="This Year"), binwidth=2, color="#e9ecef") +
  theme(
    plot.title = element_text(size=15)
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20))+
  scale_fill_manual(values = c("All Time" = clr_dist, "This Year" = clr_dist),
                    name = " ")+
  scale_color_manual(values = c("All Time" = clr_dist, "This Year" = clr_dist),
                  name = " ") +
  scale_alpha_manual(values = c("All Time" = 0.4, "This Year" = 0.9),
                     name = " ") +
  ylim(0,440) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 14), limits=c(-1,28))+
labs(title = paste0("Distance (N = ",nrow(dataset), " runs total, ",nrow(dataset_year), " this year)"),
       x = "Distance (miles)",
       y = "Count")+
  theme_pubclean()

p


clr_hist = "#007f9e";
p2 <-ggplot()+
  geom_histogram(data=dataset,aes(x=mean_pace, fill="All Time", alpha="All Time"), binwidth=1, color="#e9ecef") +
  stat_bin(data=dataset,aes(x = mean_pace, label=..count.., color="All Time", alpha="All Time"), binwidth = 1, geom="text", vjust=-1, show.legend = FALSE) +
  stat_bin(data=dataset_year,aes(x = mean_pace, label=..count..,color="This Year", alpha="This Year"), binwidth = 1, geom="text", vjust=-.3, show.legend = FALSE) +
  geom_histogram(data=dataset_year,aes(x=mean_pace, fill="This Year", alpha="This Year"), binwidth=1, color="#e9ecef") +
  theme(
    plot.title = element_text(size=15)
  ) +
  scale_fill_manual(values = c("All Time" = clr_hist, "This Year" = clr_hist),
                    name = " ")+
  scale_color_manual(values = c("All Time" = clr_hist, "This Year" = clr_hist),
                     name = " ") +
  scale_alpha_manual(values = c("All Time" = 0.4, "This Year" = 0.8),
                     name = " ") +
  labs(title = "Pace",
       x = "Pace (min/mi)",
       y = "Count")+
  ylim(0,430) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15), limits=c(4,14))+
  theme_pubclean()

p2


figure <- ggarrange(p, p2,
                    labels = c("A", "B"),
                    ncol = 1, nrow = 2)
figure

p <- dataset_year %>%
  ggplot(aes(x=season)) +
  geom_bar(stat = "count") +
  labs(x = "Month")
p


dataset_elevation_stats <- select(dataset_year,c('total_Ascent','total_Descent','total_Ascent2','total_Descent2','total_Ascent3','total_Descent3','total_Ascent4','total_Descent4','total_Ascent5','total_Descent5'))
dataset_newnames <- c("A_10","D_10","A_20","D_20","A_40","D_40","A_60","D_60","A_80","D_80")
dataset_newnames <- c("10"," 10","20"," 20","40"," 40","60"," 60","80"," 80") #This is very hacky

names(dataset_elevation_stats) <- dataset_newnames

activity_elevation_stats <- select(activities_year,c('Total.Ascent','Total.Descent'))
activity_newnames <- c("Garmin_Est"," Garmin_Est")
names(activity_elevation_stats) <- activity_newnames

dataset_elevation_stats <- gather(dataset_elevation_stats, key = "Ascent_Window", value = "Estimate")
activity_elevation_stats <- gather(activity_elevation_stats, key = "Ascent_Window", value = "Estimate")

all_elev_est <- rbind(dataset_elevation_stats,activity_elevation_stats)
all_elev_est$Estimate <- as.numeric(all_elev_est$Estimate)
all_elev_est$Ascent_Window <- as.factor(all_elev_est$Ascent_Window)

all_elev_est <- na.omit(all_elev_est)

stat.test_smooth <- all_elev_est %>%
  t_test(Estimate ~ Ascent_Window)

#manual for now
col_asc = "#7039fa"
col_des = "#a61925"
col_garm = "#3c9be8"
col_sigdiff = "gray"

cols_elev <- c(col_des,col_des,col_des,col_des,col_des,col_garm,col_asc,col_asc,col_asc,col_asc,col_asc,col_garm)
# cols_elev <- c(col_asc,col_des,col_asc,col_des,col_asc,col_des,col_asc,col_des,col_asc,col_des,col_garm,col_garm)

elevation_compare <- ggplot(all_elev_est, aes(x = reorder(Ascent_Window,-Estimate), y=Estimate, fill = Ascent_Window)) +
  geom_jitter(size=1,width = 0.2, alpha=0.08) +
  geom_violin(alpha = 0.5)+
  scale_fill_manual(values=cols_elev)+
  geom_boxplot(alpha = 0.9, width = .125) +
  labs(title = "Estimates of Elevation Gain/Loss Vary with Smoothing",
       x = "Moving Average Window Size (seconds)",
       y = "Total Ascent/Descent (ft)",
       fill = "Variable")+
  ylim(-1000,1000)+
  theme_pubclean()+
  theme(legend.position = "none")

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
  ylim(0,450)+
  stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0.01, y.position = c(420, 450, 420),bracket.shorten = 0.1, inherit.aes=FALSE)+
  theme_pubclean()+
  theme(legend.position = "none")
  # 

elevation_compare_runavg

hr_season <- ggplot(dataset, aes(x = season, y= mean_pace)) +
  geom_jitter(size=1,width = 0.35, alpha=0.14) + 
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun.y = median, fun.max = length,
               geom = "text", aes(label = paste('N =',..ymax..)), vjust = -.8)+
  labs(title = "Distribution of Multiple Columns",
       x = "Season",
       y = "Avg. Pace (min/mile)")+
  ylim(4,10)

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
