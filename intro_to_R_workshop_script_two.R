### Setting your working directory
getwd()
setwd("/Users/falvo/Desktop/Dissertation/intro_to_R_workshop")
getwd()
###

### KBS Weather Data Wrangling
library(lubridate)

dat_raw=read.csv("https://lter.kbs.msu.edu/datatables/12.csv",skip=44,header = T)
dat=dat_raw[-1,]
head(dat)
str(dat)
dat$datetime=(strptime(dat$date, "%Y-%m-%d"))
?strptime
?POSIXlt
dat$date=as.factor(strptime(dat$date, "%Y-%m-%d"))
dat$day=as.factor(yday(dat$date))
dat$month=as.factor(format(dat$datetime,"%m"))
?format
dat$year=as.factor((format(dat$datetime,"%Y")))
dat$precip=as.numeric(dat$precipitation)
dat$air_temp=as.numeric(dat$Air_temp_mean)
dat$soil_temp=as.numeric(dat$soil_temp_5_cm_sod_avg)
dat$RH=as.numeric(dat$RH)
dat$AH=as.numeric(dat$AH)
dat$wind_speed=as.numeric(dat$Wind_Speed_Mean)
dat$radiation=as.numeric(dat$Solar_Radiation)
dat$PAR=as.numeric(dat$PAR)
head(dat)

### creating a cleaned dataset 
dat_clean=as.data.frame(dat$datetime)
names(dat_clean)[1] <- "datetime"

library(ggplot2)

ggplot(dat,aes(x=as.Date(datetime),y=precip,))+
  geom_point()
#geom_histogram()
dat_clean$precip=dat$precip

ggplot(dat,aes(x=as.Date(datetime),y=air_temp))+
  geom_point()
#geom_histogram()
dat_clean$air_temp=dat$air_temp

ggplot(dat,aes(x=as.Date(datetime),y=wind_speed))+
  geom_point()
#geom_histogram()
dat_clean$wind_speed=dat$wind_speed

ggplot(dat,aes(x=as.Date(datetime),y=radiation))+
  geom_point()
#geom_histogram()
## less than 400
dat$radiation[dat$radiation>400]  <- "NA" 
dat_clean$radiation=as.numeric(dat$radiation)

ggplot(dat,aes(x=as.Date(datetime),y=as.numeric(PAR)))+
  geom_point()+
  ylim(-100,1000)
#geom_histogram()
### greater than zero and less than 750
dat$PAR[dat$PAR<0]  <- "NA" 
dat$PAR=as.numeric(dat$PAR)
dat$PAR[dat$PAR>750]  <- "NA" 
dat$PAR=as.numeric(dat$PAR)
dat_clean$PAR=as.numeric(dat$PAR)

ggplot(dat,aes(x=as.Date(datetime),y=RH))+
  geom_point()
#geom_histogram()
dat_clean$RH=dat$RH

ggplot(dat,aes(x=as.Date(datetime),y=AH))+
  geom_point()
#ylim(-1,10)
#geom_histogram()
### greater than zero less than 5 
dat$AH[dat$AH<0]  <- "NA" 
dat$AH=as.numeric(dat$AH)
dat$AH[dat$AH>5]  <- "NA" 
dat$AH=as.numeric(dat$AH)
dat_clean$AH=as.numeric(dat$AH)


ggplot(dat,aes(x=as.Date(datetime),y=soil_temp))+
  geom_point()+
  ylim(-100,100)
#geom_histogram()
## newer than 1995 older than 2016
dat$soil_temp[dat$datetime<"1995/01/01"]  <- "NA" 
dat$soil_temp=as.numeric(dat$soil_temp)
dat$soil_temp[dat$datetime>"2017/01/01"]  <- "NA" 
dat$soil_temp=as.numeric(dat$soil_temp)
dat_clean$soil_temp=as.numeric(dat$soil_temp)

dat_clean$day=as.factor(yday(dat_clean$datetime))
dat_clean$month=as.factor(format(dat_clean$datetime,"%m"))
dat_clean$year=as.factor((format(dat_clean$datetime,"%Y")))
head(dat_clean)

### computing the annual and growing season cumulative precipitation
library(dplyr)
?arrange
dat_clean= dat_clean %>% arrange(datetime)

dat_sum=dat_clean %>% 
  group_by(year) %>% 
  mutate(annual_precip = cumsum(precip))

dat_sum$precip_gs=dat_sum$precip

dat_sum$precip_gs[as.numeric(as.character(dat_sum$month))<4]  <- "NA"
dat_sum$precip_gs=as.numeric(dat_sum$precip_gs)
dat_sum$precip_gs[as.numeric(as.character(dat_sum$month))>10]  <-  "NA"
dat_sum$precip_gs=as.numeric(dat_sum$precip_gs)
dat_sum=dat_sum %>% 
  mutate(precip_gs = na_if(precip_gs, "NA"))

library(tidyr)
dat_sum=dat_sum %>% 
  group_by(year) %>% 
  mutate(cumulative_precip_gs = cumsum(replace_na(precip_gs, 0)))

gg=ggplot(dat_sum,aes(x=(day),y=annual_precip,group=year))+
  geom_point(aes(color=year))
#scale_x_date(date_labels="%d")
gg  

gg1=ggplot(dat_sum,aes(x=as.numeric(as.character(day)),y=cumulative_precip_gs,group=year))+
  geom_line(aes(color=year))+
  #geom_smooth()+
  xlim(90,310)+
  ylab("Cumulative Growing Season Precipitation (mm)\n")+
  xlab("\nDay of Year")+
  theme(
    legend.position=c(0.1, 0.75),
    legend.title = element_blank(),
    legend.box = 1,
    plot.title = element_text( size=15,hjust = 0.5),
    axis.title = element_text( size=15,color = 'Black'),
    text=element_text(size=15,color = 'Black'),
    plot.margin = margin(1,1,1,1,"cm"),
    axis.text = element_text(size=12,color = 'Black')
  )
#scale_x_date(date_labels="%d")
gg1  

gg1_1=ggplot(dat_sum,aes(x=as.numeric(as.character(day)),y=cumulative_precip_gs))+
  geom_line(aes(color=year))+
  #geom_smooth(color='black')+
  geom_quantile(size=2,color='black',
                quantiles=c(0.05,0.5,0.95))+
  #scale_linetype_manual(values="twodash")+
  xlim(90,310)+
  ylab("Cumulative Growing Season Precipitation (mm)\n")+
  xlab("\nDay of Year")+
  theme(
    legend.position=c(0.1, 0.75),
    legend.title = element_blank(),
    legend.box = 1,
    plot.title = element_text( size=15,hjust = 0.5),
    axis.title = element_text( size=15,color = 'Black'),
    text=element_text(size=15,color = 'Black'),
    plot.margin = margin(1,1,1,1,"cm"),
    axis.text = element_text(size=12,color = 'Black')
  )
#scale_x_date(date_labels="%d")
gg1_1 + geom_line(inherit.aes = F,
                  data=subset(dat_sum,year=='2021'),
                  aes(x=as.numeric(as.character(day)),
                      y=cumulative_precip_gs),size=1,color='black')

dat_sum=dat_sum %>% 
  group_by(year) %>% 
  mutate(total_cumulative_precip_gs = max(cumulative_precip_gs))


### Creating custom functions
### Finding patterns in large datasets

dat_sum$rownumber=seq(length(dat_sum$datetime))
dat_sum$groupid=cumsum(c(0,diff(replace_na(dat_sum$precip_gs, "NA")==0))!=0)
dat_sum=dat_sum %>% 
  group_by(year,groupid) %>% 
  mutate(start=first(rownumber),
         end=last(rownumber),
         number=first(precip_gs),
         size=n()
  ) 
dat_sum$size[dat_sum$size<14]  <- "NA"
dat_sum$size[dat_sum$size>50]  <- "NA"
dat_sum=dat_sum %>% 
  mutate(size = na_if(size, "NA"))

drought_function <- function(x){
  grp = rep(0,length(x))
  for(i in drought_time_period:length(x)){
    if(is.na(x[i])|is.na(x[i-(drought_time_period-1)])){
      grp[i]=0
      grp[i-(drought_time_period-1)]=0
    } else {
      if(sum(x[(i-(drought_time_period-1)):i]) <= drought_threshold){
        grp[(i-(drought_time_period-1)):i] = 1
        grp[i-drought_time_period] = 1
      } #else {
      #grp[(i-(time_period-1)):i] = FALSE
    }#}
  }
  data.frame(grp)
}

storm_function <- function(x){
  #x <- x[!is.na(x)]
  grp = rep(0,length(x))
  for(i in storm_time_period:length(x)){
    if(is.na(x[i])|is.na(x[i-(storm_time_period-1)])){
      grp[i]=0
      grp[i-(storm_time_period-1)]=0
    } else {
      if(sum(x[(i-(storm_time_period-1)):i]) >= storm_threshold){
        grp[(i-(storm_time_period-1)):i] = 1
        grp[i-storm_time_period] = 1
      } #else {
      #grp[(i-(time_period-1)):i] = FALSE
    }#}
  }
  data.frame(grp)
}

drought_time_period=6*7 # days
#rain_threshold=10 # mm
drought_threshold_percentile=0.25 # proportion growing season average
drought_threshold=drought_threshold_percentile*(drought_time_period*(mean(dat_sum$precip_gs,na.rm=T))) # mm

storm_time_period=0.1*drought_time_period # days
#storm_threshold=10 # mm
#storm_threshold_percentile=10.00 # proportion growing season average
#storm_threshold=storm_threshold_percentile*(storm_time_period*(mean(dat_sum$precip_gs,na.rm=T))) # mm
storm_threshold=(drought_time_period*(mean(dat_sum$precip_gs,na.rm=T))) # mm

dat_sum$drought=rep(0,length(dat_sum$datetime))
dat_sum$drought=(drought_function(dat_sum$precip_gs)$grp)
length(dat_sum$drought[dat_sum$drought==1])
dat_sum$storm=rep(0,length(dat_sum$datetime))
dat_sum$storm=(storm_function(dat_sum$precip_gs)$grp)
length(dat_sum$storm[dat_sum$storm==1])
#str(dat_sum)
#plot(dat_sum$drought~dat_sum$size,pch=16,col=alpha('red',0.05))

gg2=ggplot(dat_sum,aes(x=as.numeric(as.character(day)),y=drought,group=year))+
  geom_point(aes(color=year),show.legend = F)+
  ggtitle("25th Percentile 6-Week Growing Season Rainfall")+
  xlim(90,310)+
  ylim(0.9,1.2)+
  xlab("Day of Year")+
  theme(
    legend.position=c(0.85, 0.1),
    legend.title = element_blank(),
    legend.box = 1,
    plot.title = element_text( size=15,hjust = 0.5),
    axis.title = element_text( size=15,color = 'Black'),
    text=element_text(size=15,color = 'Black'),
    #plot.margin = margin(1,1,1,1,"cm"),
    axis.text = element_text(size=12,color = 'Black'),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

gg2 + facet_wrap(vars(year), nrow = 17)

gg2_1=ggplot(dat_sum,aes(x=as.numeric(as.character(day)),y=storm,group=year))+
  geom_point(aes(color=year))+
  xlim(0,365)+
  ylim(0.9,1.2)
gg2_1 + facet_wrap(vars(year), nrow = 17)

dat_sum2=dat_sum %>% 
  group_by(year) %>% 
  summarise(total_cumulative_precip_gs = max(cumulative_precip_gs),
            total_annual_precip=max(annual_precip),
            drought=sum(drought/drought_time_period),
            storm=sum(storm/storm_time_period))
dat_sum2=subset(dat_sum2,year!='2021')
dat_sum2$drought=dat_sum2$drought>0
dat_sum2$storm=dat_sum2$storm>0
head(dat_sum2,10)

write.csv(dat_sum2,file="/Users/falvo/Desktop/Dissertation/KBS_Data/KBS_Precip_Yearly_Summary.csv")

###

