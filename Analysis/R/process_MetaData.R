list.of.packages <- c('ggplot2', 'dplyr','corrplot','nloptr', 'tidyr','ggpubr', 'rstatix','RColorBrewer')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages,library, character.only=TRUE)


data_dir <- "~/Documents/Code/garmin_wrapped/Data/All_Fit_Files/all_meta"
cwd <- getwd();
setwd(data_dir);

dataset <- read.csv('full_meta_df.csv')
data_dir <- data_dir <- "~/Documents/Code/garmin_wrapped/Data/Activity Logs/";
setwd(data_dir);

activities <- read.csv("Activities_to_12_24_23.csv")
activities <- subset(activities, Activity.Type %in% c("Running","Trail Running","Track Running"))

dataset$date <- as.Date(dataset$date)
activities$Date <- as.Date(activities$Date)
activities$Total.Ascent <- as.numeric(activities$Total.Ascent)
activities$Total.Descent <- -as.numeric(activities$Total.Descent)

dataset$avg_ft_mi = dataset$total_Ascent3/dataset$distance;
dataset$avg_ft_mi = as.numeric(dataset$avg_ft_mi);

#correcting for the random number apparently specified by garmin for stance_time_balance not present
#guessing this might need to be logged if any running dynamics are logged, but
#when using the 955, no GCT balance can be collected
dataset$mean_stance_time_balance[which(dataset$mean_stance_time_balance>100)]= NA;

#also mean vertical oscillation > .5m is ludicrous, so cleaning up the v osc and ratio
#steplength > 3m is also crazy
ind_rm <- which(dataset$mean_vertical_oscillation>500);
ind_rm <- c(ind_rm, which(dataset$mean_step_length>3000));
ind_rm <- c(ind_rm, which(dataset$mean_vertical_ratio>20)); 

dataset$mean_vertical_ratio[ind_rm]= NA;
dataset$mean_vertical_oscillation[ind_rm]= NA;
dataset$mean_step_length[ind_rm]= NA;

biomechs <- select(dataset, c('mean_heart_rate','mean_pace','mean_cadence','mean_elevation','mean_step_length','mean_vertical_ratio','mean_stance_time','mean_vertical_oscillation','mean_power'))
biomechs <- sapply(biomechs, as.numeric)
colnames(biomechs) = c('Heart Rate','Pace','Cadence','Elevation','Step Length','Vertical Ratio','Stance Time','Vertical Oscillation','Power')

# activities$avg_ft_mi = activities$Total.Ascent/as.numeric(activities$Distance);
# activities$avg_ft_mi = as.numeric(activities$avg_ft_mi);

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
year_dist <- aggregate(distance ~ year, data = dataset, sum)
year_time <- aggregate(time ~ year, data = dataset, sum)
year_time$time <- year_time$time/60;

colourCount = nrow(year_time)
# getPalette = colorRampPalette(brewer.pal(9, "PuBu"))

col_dist = "cornflowerblue"
year_plot_dist <- ggplot(year_dist, aes(x = year, y = distance)) +
  geom_bar(stat = "identity",  fill = col_dist,aes(alpha=distance)) +
  geom_text(aes(label = sprintf("%.0f", distance), y = distance),  vjust = -.2, color = "gray40", size = 3.5) +
  labs(title = paste0("Distance Run Each Year (Grand Total = ", sum(as.integer(year_dist$distance))," miles)"),
       x = "Year",
       y = "Total Distance (Mi)") +
  theme_pubclean()+
  # scale_fill_manual(values = getPalette(colourCount))+
  # theme(panel.background = element_rect(fill = "gray94"))+
  theme(legend.position = "none")+
  ylim(0,1050)

# year_plot_dist

# getPalette = colorRampPalette(brewer.pal(9, "PuRd"))
col_time = "darkorchid"
year_plot_time <- ggplot(year_time, aes(x = year, y = time)) +
  geom_bar(stat = "identity", fill = col_time, aes(alpha=time)) +
  geom_text(aes(label = sprintf("%.2f", time), y = time),  vjust = -.2, color = "gray40", size = 3.5) +
  labs(title = paste0("Time Spent Running Each Year (Grand Total = ",sum(year_time$time)," hours)"),
       x = "Year",
       y = "Total Time (Hours)") +
  theme_pubclean()+
  # scale_fill_manual(values = getPalette(colourCount))+
  # theme(panel.background = element_rect(fill = "gray94"))+
  theme(legend.position = "none")+
  ylim(0,165)

# year_plot_time

year_plot <-  ggarrange(year_plot_dist, year_plot_time,
                        labels = c("A", "B"),
                        ncol = 1, nrow = 2)
year_plot


clr_dist = "#390273";
p <-ggplot()+
  geom_histogram(data=dataset,aes(x=distance, fill="All Time", alpha="All Time"), binwidth=2, color="#e9ecef") +
  stat_bin(data=dataset,aes(x = distance, label=..count.., color="All Time", alpha="All Time"), binwidth = 2, geom="text", vjust=-1.1, show.legend = FALSE) +
  stat_bin(data=dataset_year,aes(x = distance, label=..count..,color="This Year", alpha="This Year"), binwidth = 2, geom="text", vjust=-.2, show.legend = FALSE) +
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
  ylim(0,470) +
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
  stat_bin(data=dataset_year,aes(x = mean_pace, label=..count..,color="This Year", alpha="This Year"), binwidth = 1, geom="text", vjust=-.2, show.legend = FALSE) +
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
  ylim(0,470) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15), limits=c(4,14))+
  theme_pubclean()

p2


figure <- ggarrange(p, p2,
                    labels = c("A", "B"),
                    ncol = 1, nrow = 2)
figure

season_order <- c("Fall","Spring","Summer","Winter")
season_colors <- c( "#D2691E","#7EC8A4","#FFD700","#ADD8E6")

dataset_counts <- sapply(season_order, function(x) sum(grepl(x, dataset$season)))
dataset_year_counts <- sapply(season_order, function(x) sum(grepl(x, dataset_year$season)))

dataset_season_miles <- sapply(season_order, function(x) sum(dataset$distance[grepl(x, dataset$season)]))
dataset_year_season_miles <-sapply(season_order, function(x) sum(dataset_year$distance[grepl(x, dataset_year$season)]))

df_counts <- data.frame(season = season_order, counts = dataset_counts, miles = dataset_season_miles)
df_counts$percent <- 100*df_counts$counts/sum(df_counts$counts)
df_counts$miles_percent <- 100*df_counts$miles/sum(df_counts$miles)

df_counts_year <- data.frame(season = season_order, counts = dataset_year_counts, miles = dataset_year_season_miles)
df_counts_year$percent <- 100*df_counts_year$counts/sum(df_counts_year$counts)
df_counts_year$miles_percent <- 100*df_counts_year$miles/sum(df_counts_year$miles)

pie_full <- ggplot(df_counts, aes(x = "", y = miles_percent, fill = season_order)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +  # Use polar coordinates for a pie chart
  geom_text(aes(label = sprintf("%s \n %.f (%.1f%%)", season, miles, miles_percent)),
            position = position_stack(vjust = 0.5)) +
  theme_void() +  # Remove unnecessary elements
  labs(title = "12-Year Seasonal Distribution of Miles") + 
  scale_fill_manual(values=season_colors)+
  theme(legend.position = "none")

pie_yr <- ggplot(df_counts_year, aes(x = "", y = miles_percent, fill = season_order)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +  # Use polar coordinates for a pie chart
  geom_text(aes(label = sprintf("%s \n %.f (%.1f%%)", season, miles, miles_percent)),
            position = position_stack(vjust = 0.5)) +
  theme_void() +  # Remove unnecessary elements
  labs(title = "2023 Distribution") + 
  scale_fill_manual(values=season_colors)+
  theme(legend.position = "none")

both_pies <- ggarrange(pie_full, pie_yr,
                       ncol = 2, nrow = 1)
both_pies



season_plot <- ggplot() +
  geom_bar(data=dataset,aes(x=season, fill = season),stat = "count", alpha=.2) +
  geom_bar(data=dataset_year,aes(x=season, fill = season),stat="count", alpha = .8) +
  stat_count(data=dataset,aes(x = season, label=..count..), geom="text", vjust=-1, show.legend = FALSE) +
  stat_count(data=dataset_year,aes(x = season, label=..count..), geom="text", vjust=-.2, show.legend = FALSE) +
  scale_fill_manual(values=season_colors)+
  scale_x_discrete(limits = season_order)+
  labs(x = "Season")+
  # theme_pubclean()+
  theme(legend.position = "none")
  
season_plot


#Piechart version



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
  ylim(0,350)+
  stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0.01, y.position = c(325, 350, 325),bracket.shorten = 0.1, inherit.aes=FALSE)+
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


r_m <- cor(biomechs, use="complete.obs", method = "pearson");
res1 <- cor.mtest(biomechs, conf.level = 0.95);

#plot correlation matrix/clustering
corr_plot_noSig = corrplot(r_m,order='hclust');
corr_plot_Sig = corrplot(r_m,order='hclust',p.mat=res1$p, sig.level = 0.005);


setwd(cwd)
