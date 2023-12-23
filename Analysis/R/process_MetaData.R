list.of.packages <- c('ggplot2', 'dplyr','corrplot','PerformanceAnalytics','nloptr','lme4','lubridate', 'tidyr')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages,library, character.only=TRUE)


data_dir <- "~/Documents/Code/running_analytics/Data/All_Fit_Files/all_meta"
cwd <- getwd();
setwd(data_dir);

dataset <- read.csv('full_meta_df.csv')
dataset$date <- as.Date(dataset$date)
start_date <- as.Date("2022-01-01")
end_date <- as.Date("2023-12-01")

dataset_year <- dataset[dataset$date >= start_date & dataset$date <= end_date, ]

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

dataset_elevation_stats <- select(dataset,c('total_Ascent','total_Descent','total_Ascent2','total_Descent2','total_Ascent3','total_Descent3','total_Ascent4','total_Descent4','total_Ascent5','total_Descent5'))
dataset_elevation_stats <- gather(dataset_elevation_stats, key = "Ascent_Window", value = "Estimate")

elevation_compare <- ggplot(dataset_elevation_stats, aes(x = Ascent_Window, y=Estimate)) +
  geom_jitter(size=1,width = 0.35, alpha=0.14) + 
  geom_violin(alpha = 0.7) +
  facet_grid(Ascent_Window ~ ., scales = "free") +
  labs(title = "Distribution of Multiple Columns",
       x = "Value",
       y = "Density",
       fill = "Variable")

elevation_compare

setwd(cwd)
