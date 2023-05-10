##process weather data

install.packages("xts")
install.packages("tsbox")
install.packages("forecast")

library(xts)
library(tsbox)
library(forecast)
library(lubridate)


#import raw data
Slice_weather_raw1 <- read.csv("Raw/Slice_Butte_2023_05_06/SLICE_BUTTE_u30_20230506.csv",skip = 1, header = T,sep = ',', stringsAsFactors = F)
colnames(Slice_weather_raw1) <- c("obs","Datetime","Rain","Soiltemp_10cm","Soiltemp_50cm","Airtemp_2m","RH","SM_10cmC","SM_10cmB","SM_50cmC","SM_50cmB","SM_10cmA","SM_50cmA","Airtemp_3m","Airtemp_1m")
Slice_weather_raw1 <- Slice_weather_raw1 %>% mutate(Datetime = mdy_hms(Datetime))

Slice_weather_raw2 <- read.csv("Raw/Slice_Butte_2023_05_06/SLICE-2_u30_20230506.csv", skip = 1, header = T, sep = ',', stringsAsFactors = F)
colnames(Slice_weather_raw2) <- c("obs","Datetime","SM_50cmC","Rain","Airtemp_2m","RH","Soiltemp_50cm","Soiltemp_10cm","Airtemp_3m","Airtemp_1m","SM_10cmB","SM_50cmB","SM_10cmA","SM_50cmA","SM_10cmC")
Slice_weather_raw2 <- Slice_weather_raw2 %>% mutate(Datetime = mdy_hms(Datetime))

Slice_ms_sun <- read.csv("Raw/Slice_Butte_2023_05_06/slicems_20230506.csv", skip = 1, header = T, sep = ',')
colnames(Slice_ms_sun) <- c("obs","Datetime","Airtemp_1mB","RainB","Leaf_wetness_sun")
Slice_ms_sun <- Slice_ms_sun %>% mutate(Datetime = mdy_hms(Datetime))

Slice_ms_lower_SM <- read.csv("Raw/Slice_Butte_2023_05_06/slicesm_ms3_20230506.csv", skip = 1, header = T, sep = ',', stringsAsFactors = F)
colnames(Slice_ms_lower_SM) <- c("obs","Datetime","lower_SM_100cmA","lower_SM_100cmB","lower_SM_20cmA","lower_SM_20cmB")
Slice_ms_lower_SM <- Slice_ms_lower_SM %>% mutate(Datetime = mdy_hms(Datetime))

Slice_ms_shade <- read.csv("Raw/Slice_Butte_2023_05_06/slicmix_ms2_20230506.csv", skip = 1, header = T, sep = ',', stringsAsFactors = F)
colnames(Slice_ms_shade) <- c("obs","Datetime","Leaf_wetness_shade","Air_temp_1m_shade","Air_temp_1m_shade_C","RH_shade","Air_temp_1m_shade_C_B","RH_shade_B")
Slice_ms_shade <- Slice_ms_shade %>% mutate(Datetime = mdy_hms(Datetime))

Slice_ms_upper_SM <- read.csv("Raw/z6B00953 06May23-1018.csv",skip = 2, stringsAsFactors = F)
colnames(Slice_ms_upper_SM) <- c("Datetime","upper_SM_100cmA","upper_SM_100cmB","upper_SM_100cmC","upper_SM_20cmA","upper_SM_20cmB","upper_SM_20cmC","batt","batt_V")
str(Slice_ms_upper_SM)
Slice_ms_upper_SM$upper_SM_20cmA <- as.numeric(Slice_ms_upper_SM$upper_SM_20cmA)
Slice_ms_upper_SM<- Slice_ms_upper_SM %>% mutate(Datetime = mdy_hm(Datetime))


#calculate VPD from temp and RH
Slice_weather_raw1$Airtemp_2m_C <- (Slice_weather_raw1$Airtemp_2m-32)*(5/9)
Slice_weather_raw1$SVP <- 6.1078*exp((17.2694*Slice_weather_raw1$Airtemp_2m_C)/(Slice_weather_raw1$Airtemp_2m_C+273.3))
Slice_weather_raw1$VPD <- (Slice_weather_raw1$SVP-(Slice_weather_raw1$RH*(Slice_weather_raw1$SVP/100)))/10

#subset for time of interest.

Slice_weather_raw1_full <- subset(Slice_weather_raw1, Datetime < as.POSIXct("2022-10-23 00:00") & Datetime > as.POSIXct("2022-06-29 00:00"))
Slice_weather_raw2_full <- subset(Slice_weather_raw2, Datetime < as.POSIXct("2022-10-23 00:00") & Datetime > as.POSIXct("2022-06-29 00:00"))
#plot
ggplot(Slice_weather_raw1_full, aes(x=Datetime))+
  geom_line(aes(y=Airtemp_2m, color = "red5"))
  
ggplot(Slice_weather_raw1_full, aes(x=Datetime))+
  geom_line(aes(y=VPD, color = "red5"))

ggplot(Slice_weather_raw1_full, aes(x=Datetime))+
  geom_line(aes(y=Rain, color = "blue"))

#also interested in temp and RH in the shade as well as soil moisture near the tree.

Slice_shade_full <- subset(Slice_ms_shade, Datetime < as.POSIXct("2022-10-23 00:00") & Datetime > as.POSIXct("2022-06-29 00:00"))
Slice_SM_full <- subset(Slice_ms_upper_SM, Datetime < as.POSIXct("2022-10-23 00:00") & Datetime > as.POSIXct("2022-06-29 00:00"))

ggplot(Slice_shade_full, aes(x=Datetime))+
  geom_line(aes(y=Air_temp_1m_shade))

ggplot(Slice_shade_full, aes(x=Datetime))+
  geom_line(aes(y=Leaf_wetness_shade))

ggplot(Slice_SM_full, aes(x=Datetime))+
  geom_line(aes(y=upper_SM_100cmA, color = "deepA"))+
  geom_line(aes(y=upper_SM_100cmB, color = "deepB"))+
  geom_line(aes(y=upper_SM_100cmC, color = "deepC"))+
  geom_line(aes(y=upper_SM_20cmA, color = "shallowA"))+
  geom_line(aes(y=upper_SM_20cmB, color = "shallowB"))+
  geom_line(aes(y=upper_SM_20cmC, color = "shallowC"))
  
  

#compile into daily max and min for temp
Slice_daily_temp_set <- subset(Slice_weather_raw1_full, select = c("Datetime","Airtemp_2m"))
Slice_max_temp <- apply.daily(Slice_daily_temp_set, max)
Slice_min_temp <- apply.daily(Slice_daily_temp_set, min)

Slice_daily_shade_temp_set <- subset(Slice_shade_full, select = c("Datetime","Air_temp_1m_shade"))
Slice_max_temp_shade <- apply.daily(Slice_daily_shade_temp_set, max)
Slice_min_temp_shade <- apply.daily(Slice_daily_shade_temp_set, min)

#compile into daily max and min for temp
Slice_daily_VPD_set <- subset(Slice_weather_raw1_full, select = c("Datetime","VPD"))
Slice_max_VPD <- apply.daily(Slice_daily_VPD_set, max)
Slice_min_VPD <- apply.daily(Slice_daily_VPD_set, min)

#compile into daily rain totals
Slice_rain_set <- subset(Slice_weather_raw1_full, select = c("Datetime","Rain"))
Slice_rain <- apply.daily(Slice_rain_set, sum) 

#compile into daily mean SM
Slice_SM_full$deepSM <- (Slice_SM_full$upper_SM_100cmA + Slice_SM_full$upper_SM_100cmB + Slice_SM_full$upper_SM_100cmC)/3
Slice_SM_full$shallowSM <- (Slice_SM_full$upper_SM_20cmA + Slice_SM_full$upper_SM_20cmB + Slice_SM_full$upper_SM_20cmC)/3
Slice_daily_SM_deep_set <- subset(Slice_SM_full, select = c("Datetime","deepSM"))
Slice_daily_SM_shallow_set <- subset(Slice_SM_full, select = c("Datetime","shallowSM"))
Slice_dailySM_deep <- apply.daily(Slice_daily_SM_deep_set, mean)
Slicedaily_SM_shallow<- apply.daily(Slice_daily_SM_shallow_set, mean)

#compile into a datasheet for analysis
compiled_dat_2022 <- cbind(Slice_max_temp,Slice_min_temp,Slice_max_temp_shade,Slice_min_temp_shade,Slice_max_VPD, Slice_min_VPD, Slice_rain, Slice_dailySM_deep, Slicedaily_SM_shallow)
colnames(compiled_dat_2022) <- c("Max_temp_F","Min_temp_F","Max_temp_shade_F","Min_temp_shade_F", "Max_VPD_kPa","Min_VPD_kPa","Precip_in","SM_1m_m3/m3","SM_20cm_m3/m3")
#recover Datetime column
d <- compiled_dat_2022
Datetime <- rownames(d)
rownames(d) <- NULL
compiled_dat_2022 <- cbind(Datetime,d)
compiled_dat_2022 <- compiled_dat_2022 %>% mutate(Datetime = ymd_hms(Datetime))

compiled_dat_2022$doy <- yday(compiled_dat_2022$Datetime)

write.csv(compiled_dat_2022,file = "Cleaning/2022_weather_cleaned.csv")

