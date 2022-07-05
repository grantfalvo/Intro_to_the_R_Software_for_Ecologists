# Welcome to Part 2 of Intro to R Workshop

# Setup--------------------------------------------------------------------------
rm(list = ls()) # clear your environment
invisible(lapply(paste0("package:", names(sessionInfo()$otherPkgs)), detach,character.only = TRUE, unload = TRUE)) #unload user installed packages

getwd() #print current working directory
setwd("C:\Users\robin\OneDrive - Michigan State University\Robin - ConnerLab") #change working directory to new location
getwd()

# Review from Part 1--------------------------------------------------------------
# What is 56.7 x 123.4, rounded to 1 digit?
round(56.7*123.4,digits=1)

# What is the mean of 2.3, 4.5, 6.7, and 3.9?
mean(c(2.3, 4.5, 6.7, 3.9))

# In the following data frame:
df = data.frame(numbers = c(1,2,3),letters = c("a","b","c"))

# check the data type of the 2 columns
str(df)
# change the second column to a factor data type
df$letters = as.factor(df$letters)
# return the 1st row, 2nd column
df[1,2]
# return only the row in which the numbers column is >1 and the letters column does not equal "b"
df[(df$numbers>1 & df$letters!="b"),]

# Clean data -------------------------------------------------------
install.packages("lubridate") #install package for working with date data
library(lubridate) #load the package

###load in KBS weather station data
dat_raw=read.csv(file="https://lter.kbs.msu.edu/datatables/12.csv",skip=44,header = T) #read the data directly from the KBS website, skipping the 1st 44 rows that don't contain data
#for descriptions of variables, see: https://lter.kbs.msu.edu/datatables/12
View(dat_raw) #View the dataframe in an RStudio tab
dat=dat_raw[-1,] #remove row 1
head(dat) #print out first few rows of data
str(dat) #look at structure of data - what type of data is in each column? Which need to be corrected?

### change the data type of columns that are incorrect
dat$precip=as.numeric(dat$precipitation) #convert character type to numeric type
dat$air_temp=as.numeric(dat$Air_temp_mean)
dat$soil_temp=as.numeric(dat$soil_temp_5_cm_sod_avg)
dat$RH=as.numeric(dat$RH)
dat$AH=as.numeric(dat$AH)
dat$wind_speed=as.numeric(dat$Wind_Speed_Mean)
dat$radiation=as.numeric(dat$Solar_Radiation)
dat$PAR=as.numeric(dat$PAR)

dat$date=(strptime(dat$date, "%Y-%m-%d")) #convert "date" column from character to date
?strptime #bring up help page for this function
?POSIXlt

head(dat) #look at the first few rows of data
str(dat) #recheck structure

### create a clean dataset with only relevant columns
dat_clean = dat[,c(1,26:31)] #make new dataframe, keeping all rows but just a subset of columns (1 and 26 to 31)

### plot each variable to look for outliers
install.packages("ggplot2") #install package used for creating plots
library(ggplot2) 
theme_set(theme_classic()) #set aesthetic theme for all plots

ggplot(dat_clean,aes(x=as.Date(date),y=precip))+
  geom_point() #no obvious outliers

ggplot(dat_clean,aes(x=as.Date(date),y=air_temp))+
  geom_point() #no obvious outliers

ggplot(dat_clean,aes(x=as.Date(date),y=radiation))+
  geom_point() #2 outliers above 400 
dat_clean$radiation[dat_clean$radiation > 400]  <- NA #remove outliers by setting cells > 400 to NA

ggplot(dat_clean,aes(x=as.Date(date),y=soil_temp))+
  #ylim(c(-500,100))+ #use this to zoom in on certain areas of the plot
  geom_point() #6 outliers below -200
dat_clean$soil_temp[dat_clean$soil_temp < -200] <- NA #remove outliers

ggplot(dat_clean,aes(x=as.Date(date),y=soil_temp))+
  geom_point() #values before 1995 and after 2016 don't follow pattern
dat_clean$soil_temp[dat_clean$date<"1995/01/01" | dat_clean$date>"2017/01/01"]  <- NA #only keep values from 1995 to 2016, using "|" to indicate "or"

# Compute new variables from existing data--------------------------------------

### Compute new time-based variables
dat_clean$DOY=yday(dat$date) #extract day of year from date and create new column
dat_clean$month=as.numeric(format(dat$date,"%m")) #extract month as numeric value and create new column

### Compute growing season cumulative precipitation
dat_clean$precip_gs=dat_clean$precip #copy precipitation column to a new column
dat_clean$precip_gs[dat_clean$month<4 | dat_clean$month>10] <- NA #only keep months 4-10 (April - Oct)

#check that precip_gs is correct by sorting the dataframe by month
install.packages("dplyr") #dataframe manipulation package
library(dplyr)
?arrange
dat_clean= dat_clean %>% arrange(date) #sort by date

install.packages("tidyr") #data reshaping package
library(tidyr) 
dat_clean <- dat_clean %>% #load the dataframe, use "%>%" to indicate what to do next with that dataframe
  group_by(Year) %>% #group the data by year
  mutate(cumulative_precip_gs = cumsum(replace_na(precip_gs, 0))) #create a new variable by taking the cumulative sum of precip_gs, after replacing NA with 0

ggplot(dat_clean,aes(x=DOY,y=cumulative_precip_gs))+
  geom_line(aes(color=as.factor(Year)))

# Summarize data by group/s----------------------------------------------------

### Summarize precipitation and air temperature by year
dat_sum=dat_clean %>% 
  group_by(Year) %>% #data grouped by year
  filter(Year!=2022)%>% #exclude 2022 since data collection is not complete yet
  summarize(total_cumulative_precip_gs = max(cumulative_precip_gs), #max cumulative precipitation
            mean_air_temp = mean(air_temp,na.rm=T), #mean air temp
            sd_air_temp = sd(air_temp,na.rm=T), #standard deviation for air temp
            N = n(), #number of days included
            se_air_temp = sd_air_temp/sqrt(N)) #standard error for air temp

ggplot(dat_sum,aes(x=Year,y=total_cumulative_precip_gs))+
  geom_point()

ggplot(dat_sum,aes(x=Year,y=mean_air_temp))+
  geom_point()+
  geom_errorbar(aes(ymin=mean_air_temp - se_air_temp,ymax=mean_air_temp + se_air_temp)) #add SE errorbars to plot

### Output R dataframe to your computer
write.csv(dat_sum,file="KBS_Precip_Yearly_Summary.csv") #output as csv file, goes to your working directory by default
write.csv(dat_sum,file="C:/Users/robin/Downloads/KBS_Precip_Yearly_Summary.csv") #You can also include full directory name before file name to send to specific location
write.table(dat_sum,"clipboard",sep="\t",row.names=F) #you can also copy a dataframe to your clipboard and paste into Excel

# Run statistical models--------------------------------------------------------

### test for a significant correlation between mean annual air temperature and total cumalative growing season precipitation
cor.test(dat_sum$mean_air_temp, dat_sum$total_cumulative_precip_gs,method = "pearson") #not significantly correlated

### test if the mean annual air temperature at KBS has increased over time
air_temp_lm = lm(mean_air_temp~Year,dat_sum)
summary(air_temp_lm)
coef(air_temp_lm) #extract coefficients from regression
coef(air_temp_lm)[2]*(2021-1989) #how much air temp has changed between 1989 and 2021
ggplot(dat_sum,aes(x=Year,y=mean_air_temp))+
  geom_point()+
  geom_errorbar(aes(ymin=mean_air_temp - se_air_temp,ymax=mean_air_temp + se_air_temp))+
  geom_abline(intercept=coef(air_temp_lm)[1],slope=coef(air_temp_lm)[2],color="red") #add in regression line computed in model

### test if relationship remains after accounting for precipitation
temp_precip_lm = lm(mean_air_temp ~ Year + total_cumulative_precip_gs,dat_sum)
summary(temp_precip_lm)


### test affects of two independent variables and their interaction
data("ToothGrowth") #load built-in data on effects of vitamin C doses and supplement types ("supp") on guinea pig tooth length
tooth_dat = ToothGrowth
ggplot(aes(x=dose,y=len,fill=supp),data=tooth_dat)+
  geom_bar(stat="identity",position=position_dodge())
tooth_lm= lm(len ~ dose*supp, tooth_dat) #var1*var2 is equivalent to var1+var2+interaction(var1 & var2)
summary(tooth_lm) #tooth length is significantly affected by dose, supplement type, and their interaction

### use a statistical test on a subset of the data
#subset to just the VC supplement type
#using base R indexing:
VC_lm = lm(len ~ dose, data = tooth_dat[tooth_dat$supp=="VC",])
#using filter function from dplyr:
VC_lm = lm(len ~ dose, data = tooth_dat %>% filter(supp == "VC"))

summary(VC_lm)

### logistic regression for binary dependent variable
credit_dat = read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/ISLR/Default.csv") #download dataset on customers defaulting on credit card debt
credit_dat$default = recode(credit_dat$default,Yes=1,No=0)
credit_glm <- glm(default~student,data= credit_dat, family="binomial") #is defaulting affected by a customer's average balance?
summary(credit_glm)
exp(coef(credit_glm)) #being a student increases the chances of defaulting by 1.5 times, or 50%


######################## Group Activity ##############################
#______________ KBS Resource Gradient Experiment_______________________

#Read in data from the KBS Resource Gradient Experiment (For details, see https://lter.kbs.msu.edu/datatables/77. Clean the data, summarize the data, and run a statistical test.

### Read data and change data structure
dat=read.csv(header=T,skip=30,"https://lter.kbs.msu.edu/datatables/77.csv")
dat=dat[-1,]
head(dat)
dat$irrigated=as.factor(dat$irrigated)
dat$fertilizer_num=as.numeric(dat$fertilizer_rate_kg_ha)
dat$fertilizer_fac=as.factor(dat$fertilizer_rate_kg_ha)
dat$crop=as.factor(dat$crop)
dat$yield=as.numeric(dat$yield_kg_ha)
dat$irrigated <- recode_factor(dat$irrigated, t = "Irrigated", f = "Rainfed")
dat$date=(strptime(dat$date, "%Y-%m-%d"))
dat$treatment=as.factor(dat$treatment)
dat$location=as.factor(dat$location)
str(dat)

### Check for outliers
ggplot(dat,aes(x=as.Date(date),y=yield))+
  geom_point() #no outliers

### Summarize data by crop, fertilizer rate, and irrigation
dat_sum = dat %>% 
  group_by(crop,fertilizer_num,irrigated) %>% 
  summarize(N=n(),
            mean_yield=mean(yield,na.rm=T),
            sd_yield=sd(yield,na.rm=T),
            se_yield = sd_yield/sqrt(N))

### Visualize summarized data
ggplot(dat_sum,aes(x=fertilizer_num,y=mean_yield,
               color=irrigated))+
  geom_point()+
  geom_smooth()+
  geom_errorbar(aes(ymin=mean_yield - se_yield,ymax=mean_yield + se_yield))+
  facet_wrap(~crop)

### Use a statistical model to test which variables affect yield
datlm=lm(yield~crop*fertilizer_num*irrigated,dat)
library(car)
Anova(datlm,type=3) 
summary(datlm)
#Overall, rainfed plots had lower yield and Zea mays plots had higher yield. The affects of fertilizer and irrigation also vary by crop, and the affect of irrigation varies by fertilizer treatment. 

### Use a statistical model on just a subset of the data - a single crop type
datlm_Zea=lm(yield~fertilizer_num*irrigated,
         data=dat%>%
           filter(crop=='Zea mays L. (*)'))
Anova(datlm_Zea,type=3)
summary(datlm_Zea)
#In Zea mays, overall, plots that were irrigated and have increasing amounts of fertilizer had greater yield. Also, the affect of irrigation increased with increasing levels of fertilizer

ggplot(dat%>%
       filter(crop=='Zea mays L. (*)'),
       aes(x=fertilizer_num,y=yield))+
  geom_point()+
  geom_smooth()+
  facet_wrap(vars(irrigated),nrow=1)








