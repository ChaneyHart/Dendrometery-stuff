### Analysis of stem dendrometer growth and tree water deficit
install.packages("ggtext")

library(GGally)
library(xts)
library(lubridate)
library(dplyr)
library(reshape2)
library(RColorBrewer)
library(gridExtra)
library(ggtext)

#read in dendrometry data


d133 <- read.csv(file = "Cleaning/dendro_133_cleaned.csv")
d133 <- d133 %>% mutate(ts = ymd_hms(ts))
d140 <- read.csv(file = "Cleaning/dendro_140_cleaned.csv")
d140 <- d140 %>% mutate(ts = ymd_hms(ts))

#trim 

d133 <- subset(d133, select = c("series","ts","value","max","twd","gro_yr","yr"))
d140 <- subset(d140, select = c("series","ts","value","max","twd","gro_yr","yr"))

#Calculate daily growth rate (may want to convert to a function later)
d133_growth_set <- subset(d133, select = c("ts","gro_yr"))
d133_growth_max <- apply.daily(d133_growth_set, max)
d133_growth_min <- apply.daily(d133_growth_set, min)
d133_growth_daily <- cbind(d133_growth_max,d133_growth_min)
colnames(d133_growth_daily) <- c("max_growth","min_growth")
d133_growth_daily$Datetime <- as.POSIXct(row.names(d133_growth_daily))
d133_growth_daily$gro_rate <- d133_growth_daily$max_growth - d133_growth_daily$min_growth
d133_growth_daily$doy <- yday(d133_growth_daily$Datetime)

#Calculate cumulative daily twd
d133_twd_set <- subset(d133, select = c("ts","twd"))
d133_twd_mean <- apply.daily(d133_twd_set, mean)
colnames(d133_twd_mean) <- c("mean_twd")
d133_twd_mean$Datetime <- as.POSIXct(row.names(d133_twd_mean))
d133_twd_mean$doy <- yday(d133_twd_mean$Datetime)

#combine gro rate and twd
d133_daily_stats <- inner_join(d133_growth_daily, d133_twd_mean, by = "doy")
d133_daily_stats$series <- "133"

####repeat for d140 #######

#Calculate daily growth rate (may want to convert to a function later)
d140_growth_set <- subset(d140, select = c("ts","gro_yr"))
d140_growth_max <- apply.daily(d140_growth_set, max)
d140_growth_min <- apply.daily(d140_growth_set, min)
d140_growth_daily <- cbind(d140_growth_max,d140_growth_min)
colnames(d140_growth_daily) <- c("max_growth","min_growth")
d140_growth_daily$Datetime <- as.POSIXct(row.names(d140_growth_daily))
d140_growth_daily$gro_rate <- d140_growth_daily$max_growth - d140_growth_daily$min_growth
d140_growth_daily$doy <- yday(d140_growth_daily$Datetime)

#Calculate cumulative daily twd
d140_twd_set <- subset(d140, select = c("ts","twd"))
d140_twd_mean <- apply.daily(d140_twd_set, mean)
colnames(d140_twd_mean) <- c("mean_twd")
d140_twd_mean$Datetime <- as.POSIXct(row.names(d140_twd_mean))
d140_twd_mean$doy <- yday(d140_twd_mean$Datetime)

#combine gro rate and twd
d140_daily_stats <- inner_join(d140_growth_daily, d140_twd_mean, by = "doy")
d140_daily_stats$series <- "140"


###combine dendros 133 and 140###
combined_dendro_stats <- rbind(d133_daily_stats,d140_daily_stats)



#read in weather dat
Slice_weather_summary <- read.csv(file = "Cleaning/2022_weather_cleaned.csv")


##compile dendro stats and weather##

Slice_analysis <- inner_join(Slice_weather_summary, combined_dendro_stats, by = "doy")
Slice_analysis$series <- as.factor(Slice_analysis$series)
#2022 growing season, variables of interest
Slice_analysis_2022 <- subset(Slice_analysis, Datetime.x < as.POSIXct("2022-10-23 00:00") & Datetime.x > as.POSIXct("2022-06-29 00:00"))
Slice_analysis_2022 <- subset(Slice_analysis_2022, doy > 181)
Slice_analysis_2022 <- subset(Slice_analysis_2022, select = c("Max_temp_F","Max_temp_shade_F","Max_VPD_kPa","Precip_in","SM_1m_m3.m3","SM_20cm_m3.m3","doy","gro_rate","mean_twd","series"))


Slice_analysis_2022_long <- melt(Slice_analysis_2022, id = c("doy","series"))
#Slice_analysis_2022_long <- Slice_analysis_2022_long %>% mutate(Datetime = ymd_hms(Datetime))
#plot

Slice_summary_plot <- ggplot(Slice_analysis_2022_long, aes(x=doy, y=value, color = series))+
  geom_line()

Slice_summary_plot + facet_grid(variable ~ ., scales = "free_y")

#useful for full visualization of correlations

Slice_analysis_corrplot_gro <- ggpairs(subset(Slice_analysis_2022, select = c("Max_temp_F","Max_VPD_kPa","Max_temp_shade_F","SM_1m_m3.m3","SM_20cm_m3.m3","gro_rate")))
ggsave(filename = "Graph/Slice_analysis_corrplot_gro.png", plot =Slice_analysis_corrplot_gro, width = 8, height = 11, units = "in", dpi = 300)


Slice_analysis_corrplot_twd <- ggpairs(subset(Slice_analysis_2022, select = c("Max_temp_F","Max_VPD_kPa","Max_temp_shade_F","SM_1m_m3.m3","SM_20cm_m3.m3","mean_twd")))
ggsave(filename = "Graph/Slice_analysis_corrplot_twd.png", plot =Slice_analysis_corrplot_twd, width = 8, height = 11, units = "in", dpi = 300)


#Plotting interesting correlations


gro_1 <- ggplot(Slice_analysis_2022, aes(x=Max_VPD_kPa, y = gro_rate, shape = series))+
  geom_point(aes(color = SM_20cm_m3.m3))+
  xlab("Canopy VPD (daily max kPa)")+
  ylab("Daily growth rate (µm/day)")+
  scale_color_gradientn(colors = rainbow(3))+
  theme_bw()+
  theme(legend.position="none")
  

gro_1 


gro_2 <- ggplot(Slice_analysis_2022, aes(x=Max_temp_F, y = gro_rate, shape = series))+
  geom_point(aes(color = SM_20cm_m3.m3))+
  xlab("Canopy temp (daily max ˚F)")+
  ylab("Daily growth rate (µm/day)")+
  scale_color_gradientn(colors = rainbow(3))+
  theme_bw()+
  theme(legend.position="none")
  

gro_2

gro_3 <- ggplot(Slice_analysis_2022, aes(x=Max_VPD_kPa, y = mean_twd, shape = series))+
  geom_point(aes(color = SM_20cm_m3.m3))+
  xlab("Canopy VPD (daily max kPa)")+
  ylab("Mean daily tree water deficit (µm)")+
  scale_color_gradientn(colors = rainbow(3))+
  theme_bw()+
  theme(legend.position="none")
  

gro_3

gro_4 <- ggplot(Slice_analysis_2022, aes(x=Max_temp_F, y = mean_twd, shape = series))+
  geom_point(aes(color = SM_20cm_m3.m3))+
  xlab("Canopy temp (daily max ˚F)")+
  ylab("Mean daily tree water deficit (µm)")+
  scale_color_gradientn(colors = rainbow(3))+
  theme_bw()+
  theme(legend.position="none")
  

gro_4


Corr_figure <- grid.arrange(gro_1,gro_2,gro_3,gro_4, nrow =1)
ggsave(filename = "Graph/Correlations_figure.png", plot = Corr_figure, width = 12, height = 3, units = "in", dpi=300)


gro_legend <- ggplot(Slice_analysis_2022, aes(x=Max_temp_F, y = mean_twd, shape = series))+
  geom_point(aes(color = SM_20cm_m3.m3))+
  xlab("Canopy temp (daily max ˚F)")+
  ylab("Mean daily tree water deficit (µm)")+
  scale_color_gradientn(colors = rainbow(3), name = "Soil moisture (daily m<sup>3</sup>/m<sup>3</sup>)")+
  theme_bw()+
  theme(legend.title = element_markdown())
  

gro_legend  

ggsave(filename = "Corr_plot_legend.png", plot = gro_legend, dpi = 300)


  











  