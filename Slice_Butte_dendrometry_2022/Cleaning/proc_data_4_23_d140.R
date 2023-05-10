##R script for reading and cleaning dendrometer data and exporting analysis files

# install if necessary
packages <- (c("devtools","zoo","chron","dplyr","viridis", "RCurl", "DT","lubridate"))
install.packages(setdiff(packages, rownames(installed.packages())))
devtools::install_github("treenet/treenetproc")


library(treenetproc)
library(zoo)
library(chron)
library(viridis)
library(dplyr)
library(ggplot2)
library(lubridate)

getwd()

# helper functions
left <-  function(string, char){substr(string, 1,char)}
right <-  function (string, char){substr(string,nchar(string)-(char-1),nchar(string))}


####### read in dendrometer data and plot raw data ############
ID = '140' #dendrometer ID number
all_data_140<-read.table('Raw/data_92223140_2023_04_23_0.csv', header=F, sep=";", dec=',')
str(all_data_140)
dendro_data_140 = all_data_140[,c(2,7)]
colnames(dendro_data_140) <- c('datetime','value')

#fix timestamp
ts = as.POSIXct(dendro_data_140['datetime'][,],format="%Y.%m.%d %H:%M",tz="GMT")
d140_L0<-cbind(dendro_data_140["value"]+0,ts,series=ID)

#plot raw data

# first grab years string
years <-left(d140_L0[,"ts"],4)

# Then plot
#set number of plots 
par(mfrow=c(1,1))
#set margins
par(mar = c(5, 5, 5, 5))

# for loop plots data iteratively.
for(y in 1:length(unique(years))){
  # selected year
  sel<-d140_L0[which(years==unique(years)[y]),]
  # handle first year
  if(y==1){
    plot(difftime(as.POSIXct(sel$ts,format="%Y-%m-%d %H:%M:%S",tz="GMT"),
                  as.POSIXct(paste0(unique(years)[y],"-01-01 00:00:00"),
                             format="%Y-%m-%d %H:%M:%S",tz="GMT"),
                  units = "days"),
         sel$value,
         ylab=expression("L0 ("*mu*"m)"),
         xlab="Day of year",type="l",
         col=viridis(length(unique(years)))[y],
         xlim=c(0,365),
         ylim=c(min(d140_L0$value,na.rm=T),
                max(d140_L0$value,na.rm=T)),
         main=unique(d140_L0$series))
    
    legend("bottomright",
           as.character(unique(years)[-4]),
           col=viridis(length(unique(years))),
           bty="n",lty=1)
    # add other years
  }else{
    lines(difftime(as.POSIXct(sel$ts,format="%Y-%m-%d %H:%M:%S",tz="GMT"),
                   as.POSIXct(paste0(unique(years)[y],"-01-01 00:00:00"),
                              format="%Y-%m-%d %H:%M:%S",tz="GMT"),
                   units = "days"),
          sel$value,
          col=viridis(length(unique(years)))[y])}
}


###Now to cleaning the dendrometery data##########

# level 1 processing of dendrometer readings (time align)
?treenetproc::proc_L1
d140_L1 <- proc_L1(data_L0 = d140_L0,
                   reso = 60 ,
                   #input = "wide",
                   date_format ="%Y-%m-%d %H:%M:%S",
                   tz = "GMT")
head(d140_L1)

# level 1 processing of temperature data
temp_data_140_L0 <- cbind(all_data_140[,c(4,6)],ts,series=ID)
colnames(temp_data_140_L0) <-c('value','empty','ts','series')

# time-align temperature data with proc_L1
temp_data_140_L1 <- proc_L1(data_L0 = temp_data_140_L0,
                            reso = 60,
                            date_format ="%Y-%m-%d %H:%M:%S",
                            tz = "GMT")
head(temp_data_140_L1)


## proc_dendro_L2 integrates dendrometer readings with temperature, detects errors, corrects them and plots gro and twd by date.
#more info here:
?treenetproc::proc_dendro_L2
## first plot full dataset for dendrometer. Plot will be exported to working directory as pdf


d140_L2 <- proc_dendro_L2(dendro_L1 = d140_L1,
                          temp_L1 = temp_data_140_L1,
                          tol_out = 2,
                          tol_jump = 3,
                          plot = TRUE,
                          frost_thr = 1,
                          plot_period = "full",
                          plot_export = TRUE,
                          interpol = 3.5*60,
                          frag_len = NULL,
                          plot_name = "Slice_butte_2022_23_140_full",
                          tz="GMT")
graphics.off()

## Automated error detection did pretty well.
## Tolerance for outliers and tolerance for jumps were set lower (more stringent) than default value.

#plot monthly and look at exported pdf for greater detail. 

d140_L2_monthly <- proc_dendro_L2(dendro_L1 = d140_L1,
                                  temp_L1 = temp_data_140_L1,
                                  tol_out = 2,
                                  tol_jump = 3,
                                  frost_thr = 1,
                                  plot = TRUE,
                                  interpol = 3.5*60,
                                  plot_period = "monthly",
                                  plot_export = TRUE,
                                  plot_name = "Slice_butte_2022_23_140_monthly",
                                  tz="GMT")
graphics.off()

#Can manually correct some areas where automated data cleaning was not up to snuff
# First off, delete data from before installation on June 22st 2022 and the first week after installation
#info on function here
?corr_dendro_L2

d140_L2_corr1 <- corr_dendro_L2(dendro_L1 = d140_L1,
                                dendro_L2 = d140_L2,
                                delete = c("2022-05-10","2022-06-29"),
                                n_days = 2,
                                plot = TRUE,
                                plot_export = TRUE,
                                tz="GMT")

##check errors in pdf and with this subset
d140_L2_corr1[which((d140_L2_corr1$flags)!="NA"),]

##plot
ggplot((subset(d140_L2_corr1, frost == "FALSE")), aes(x=ts))+
  geom_line(aes(y=value), color = "grey70")+
  geom_line(aes(y=gro_yr), color = "seagreen")+
  geom_line(aes(y=twd), color = "red")

#ok but may want to visualize cumulative growth instead of growth for each year.


#create column for separate years
d140_L2_corr1$yr <- substr(d140_L2_corr1$ts,1,4)

# create seperate dataframes for each year 

d140_2023_growth <- d140_L2_corr1[which(d140_L2_corr1$yr == "2023"),]
d140_2022_growth <- d140_L2_corr1[which(d140_L2_corr1$yr == "2022"),]

#for 2023, add that years growth to 2022's maximum growth of 2214 µm 
#for 2022, make a copy of gro_yr

d140_2023_growth$gro_tot <- d140_2023_growth$gro_yr + 2214
d140_2022_growth$gro_tot <- d140_2022_growth$gro_yr 

#recombine both years data frames into one.
d_140_L3 <- rbind(d140_2022_growth, d140_2023_growth)

## plot
ggplot((subset(d_140_L3, frost == "FALSE")), aes(x=ts))+
  geom_line(aes(y=value), color = "grey70")+
  geom_line(aes(y=gro_tot), color = "seagreen")+
  geom_line(aes(y=twd), color = "red")

## just look at the growing season

ggplot((subset(d_140_L3, ts < as.POSIXct("2022-10-23 00:00") & ts > as.POSIXct("2022-06-29 00:00"))), aes(x=ts))+
  geom_line(aes(y=value), color = "grey70")+
  geom_line(aes(y=gro_tot), color = "seagreen")+
  geom_line(aes(y=twd), color = "red")


##The function phase_stats will compute rates of expansion(growth) and shrinkage (twd) and aggregate these on a daily basis. 
?phase_stats

d140_phase <- phase_stats(d140_L2_corr1,
                          plot_phase = TRUE,
                          plot_export = TRUE,
                          agg_daily = TRUE,
                          tz = 'GMT'
)

## These growth and shrink rates can be compared with temperature, VPD and soil moisture.
## We will likely restrict this analysis to the growing season.

#some preliminary analysis/exploration
ggplot(subset(d140_phase, doy > 180 & doy < 250), aes(x=doy))+
  geom_point(aes(y = shrink_amp, colour = "Shrinkage" ))+
  geom_point(aes(y = exp_amp, colour ="Expansion"))+
  ylab("Amplitude")

ggplot(subset(d140_phase, doy > 180 & doy < 250), aes(x=doy))+
  geom_point(aes(y = shrink_slope, colour = "Shrinkage" ))+
  geom_point(aes(y = exp_slope, colour ="Expansion"))+
  ylab("Slope")       

ggplot(subset(d140_phase, doy > 180 & doy < 250), aes(x=shrink_amp, y =  exp_amp))+
  geom_point()+
  ylab(" Expansion amplitude")+
  xlab(" Shrinkage amplitude")

ggplot(subset(d140_phase, doy > 180 & doy < 250), aes(x=shrink_slope, y =  exp_slope))+
  geom_point()+
  ylab(" Expansion slope")+
  xlab(" Shrinkage slope")

##now that data is cleaned, export for analysis with weather data

#for plotting
write.csv(file = "Cleaning/dendro_140_cleaned.csv", d_140_L3)

#daily growth stats
write.csv(file = "Cleaning/dendro_140_phase_stats.csv", d140_phase)




