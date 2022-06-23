### clear your environment
rm(list = ls())
### unload user installed packages 
invisible(lapply(paste0("package:", names(sessionInfo()$otherPkgs)), detach,character.only = TRUE, unload = TRUE))

### Setting your working directory
getwd()
setwd("/Users/falvo/Desktop/Dissertation/intro_to_R_workshop")
getwd()
###

### KBS Weather Data Wrangling
library(lubridate)

dat_raw=read.csv(file="https://lter.kbs.msu.edu/datatables/12.csv",skip=44,header = T)
dat=dat_raw[-1,]
head(dat)
str(dat)
dat$date=(strptime(dat$date, "%Y-%m-%d"))
?strptime
?POSIXlt
dat$DOY=yday(dat$date)
dat$month=format(dat$datetime,"%m")
?format
dat$year=format(dat$datetime,"%Y")
dat$precip=as.numeric(dat$precipitation)
dat$air_temp=as.numeric(dat$Air_temp_mean)
dat$soil_temp=as.numeric(dat$soil_temp_5_cm_sod_avg)
dat$RH=as.numeric(dat$RH)
dat$AH=as.numeric(dat$AH)
dat$wind_speed=as.numeric(dat$Wind_Speed_Mean)
dat$radiation=as.numeric(dat$Solar_Radiation)
dat$PAR=as.numeric(dat$PAR)
head(dat)
str(dat)

### creating a cleaned dataset 
dat_clean=as.data.frame(dat$date)
names(dat_clean)[1] <- "date"

library(ggplot2)
theme_set(theme_classic())

ggplot(dat,aes(x=as.Date(date),y=precip))+
  geom_point()
dat_clean$precip=dat$precip

ggplot(dat,aes(x=as.Date(date),y=air_temp))+
  geom_point()
dat_clean$air_temp=dat$air_temp

ggplot(dat,aes(x=as.Date(date),y=radiation))+
  geom_point()
## less than 400
dat$radiation[dat$radiation>400]  <- NA
dat_clean$radiation=dat$radiation

ggplot(dat,aes(x=as.Date(date),y=AH))+
  geom_point()
  coord_cartesian(ylim=c(-1,10))
#geom_histogram()
### greater than 100
dat$AH[dat$AH>100]  <- NA
dat_clean$AH=dat$AH

ggplot(dat,aes(x=as.Date(date),y=soil_temp))+
  geom_point()+
  ylim(-100,100)
#geom_histogram()
## newer than 1995 older than 2016
dat$soil_temp[dat$date<"1995/01/01"]  <- NA
dat$soil_temp[dat$date>"2017/01/01"]  <- NA 
dat_clean$soil_temp=dat$soil_temp

dat_clean$DOY=yday(dat_clean$date)
dat_clean$month=as.numeric(format(dat_clean$date,"%m"))
dat_clean$year=as.numeric(format(dat_clean$date,"%Y"))
head(dat_clean)

### computing the annual and growing season cumulative precipitation
library(dplyr)
?arrange
dat_clean= dat_clean %>% arrange(date)

dat_clean$precip_gs=dat_clean$precip
dat_clean$precip_gs[dat_clean$month<4]  <-NA
dat_clean$precip_gs[dat_clean$month>10]  <-  NA

library(tidyr)
dat_sum=dat_clean %>% 
  group_by(year) %>% 
  mutate(cumulative_precip_gs = cumsum(replace_na(precip_gs, 0)))

ggplot(dat_sum,aes(x=as.Date(DOY,origin='2022-01-01'),
                   y=cumulative_precip_gs))+
  geom_point(aes(color=as.factor(year)))+
  scale_x_date(date_labels="%b")

dat_sum_2=dat_sum %>% 
  group_by(year) %>% 
  filter(year!=2022)%>%
  summarise(total_cumulative_precip_gs = max(cumulative_precip_gs),
            mean_air_temp=mean(air_temp,na.rm=T))

ggplot(dat_sum_2,aes(x=year,y=total_cumulative_precip_gs))+
  geom_point()

ggplot(dat_sum_2,aes(x=year,y=mean_air_temp))+
  geom_point()

### output your new dataset as a csv file to your working directory
getwd()
paste0(getwd(),'/KBS_Precip_Yearly_Summary.csv')
#write.csv(dat_sum_2,file=paste0(getwd(),'/KBS_Precip_Yearly_Summary.csv'))

### construct a linear model to test if the mean annual air tempearture
### at KBS has increased over the years
datlm=lm(mean_air_temp~year,dat_sum_2)
anova(datlm)
hist(resid(datlm))
shapiro.test(resid(datlm))
summary(datlm)
coef(datlm)
coef(datlm)[2]
coef(datlm)[2]*(2021-1989)

###

######################################################################
######################## Group Activity ##############################
######################################################################

######################################################################
############## KBS Resource Gradient Experiment #####################
######################################################################

dat=read.csv(header=T,skip=30,"https://lter.kbs.msu.edu/datatables/77.csv")
dat=dat[-1,]
head(dat)
dat$irrigated=as.factor(dat$irrigated)
dat$fertilizer_num=as.numeric(dat$fertilizer_rate_kg_ha)
dat$fertilizer_fac=as.factor(dat$fertilizer_rate_kg_ha)
dat$crop=as.factor(dat$crop)
dat$yield_C=as.numeric(dat$yield_kg_ha)*0.45*(1/1000) ### Mg C per ha units
str(dat)

dat$irrigated <- recode_factor(dat$irrigated, t = "Irrigated", 
                                f = "Rainfed")

KBS_Precip_Yearly_Summary=read.csv(file=paste0(getwd(),'/KBS_Precip_Yearly_Summary.csv'))
KBS_Precip_Yearly_Summary$X=NULL
str(KBS_Precip_Yearly_Summary)

dat=merge(dat,KBS_Precip_Yearly_Summary,by='year',all.x=T)
str(dat)

ggplot(dat,aes(x=fertilizer_num,y=yield_C,
               color=as.factor(year),fill=as.factor(year)))+
  geom_point(stat='summary',fun=mean, shape=21,size=3)+
  geom_smooth(se=F)+
  geom_hline(yintercept = 0)+
  facet_wrap(vars(irrigated,crop),nrow=2)

datlm=lm(yield_C~crop*fertilizer_num*irrigated,dat)
anova(datlm)
summary(datlm)
hist(resid(datlm))
shapiro.test(resid(datlm))

unique(dat$crop)
unique(dat$fertilizer_num)

ggplot(dat%>%
         filter(crop=='Glycine max L. (*)' & fertilizer_num>0),
         #filter(crop=='Zea mays L. (*)' & fertilizer_num==291),
       aes(x=fertilizer_num,y=yield_C))+
  geom_point(aes(fill=as.factor(crop)),
             shape=21,size=3)+
  geom_smooth(se=F,method='lm')+
  geom_hline(yintercept = 0)+
  facet_wrap(vars(irrigated,crop),nrow=1)

datlm=lm(yield_C~fertilizer_num*irrigated,
         data=dat%>%
           filter(crop=='Glycine max L. (*)' & fertilizer_num>0))
anova(datlm)
summary(datlm)
hist(resid(datlm))
shapiro.test(resid(datlm))






