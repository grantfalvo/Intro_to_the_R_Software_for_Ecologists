### clear your environment
rm(list = ls())
### unload user installed packages 
invisible(lapply(paste0("package:", names(sessionInfo()$otherPkgs)), detach,character.only = TRUE, unload = TRUE))

library(tidyverse)
dat_raw=read.csv(header=T,skip=29,file='https://lter.kbs.msu.edu/datatables/39.csv')
dat=dat_raw[-1,]
dat
head(dat)
str(dat)
dat$Year=as.numeric(dat$Year)
dat=subset(dat,dat$Year>=1993)
dat$Treatment=as.factor(dat$Treatment)
dat$Replicate=as.factor(dat$Replicate)
dat$Station=as.factor(dat$Station)
dat$Species=as.factor(dat$Species)
dat$Fraction=as.factor(dat$Fraction)
str(dat)

dat=na.omit(dat)
unique(dat$Treatment)
dat=dat%>%
  filter(Treatment!='T21' & Treatment!='T6')%>%
  filter(Species == "Glycine max L. (*)" | 
         Species == "Triticum aestivum L. (*)" | 
         Species == "Zea mays L. (*)" )%>%
  filter(!is.na(Biomass))
dat$Species <- recode_factor(dat$Species, 
                             'Glycine max L. (*)' = "Soybean", 
                             "Triticum aestivum L. (*)" = "Wheat",
                             "Zea mays L. (*)" = 'Corn')
library(plotrix)
dat_sum=group_by(dat, Year, Treatment,Species,Fraction) %>% 
  summarise(yield=mean(Biomass),yield_se=std.error(Biomass))
dat_sum

hist(dat$Biomass)
ggplot(dat,aes(x=Biomass))+
  geom_histogram()

boxplot(Biomass~Species,dat)
ggplot(dat,aes(x=Species,y=Biomass))+
  geom_boxplot()

barplot(yield~Year,dat_sum %>% 
          filter(Species=='Corn' & Treatment=='T1' & Fraction=='SEED'))
ggplot(dat_sum %>% 
         filter(Species=='Corn' & Treatment=='T1' & Fraction=='SEED'),
       aes(x=Year,y=yield))+
  geom_bar(stat='identity')

plot(Biomass~Year,dat)
ggplot(dat,aes(x=Year,y=Biomass))+
  geom_point()

### ggplot
ggplot(dat_sum%>%
         filter(Fraction=='SEED'),
           aes(y=yield,x=Year,group=Treatment))+
  facet_wrap(vars(Species), nrow = 1)+
  geom_point(aes(color=Treatment),size=5)+
  geom_errorbar(aes(ymin  =  yield-yield_se, ymax  =  yield+yield_se),
                width =  0.3, size  =  0.5)+
  geom_smooth(method='lm',aes(color=Treatment),se=F)+
  ylab(bquote("Harvested Grain"~g~m^-2))+
  xlab("Year")+
  ggtitle('KBS Crop Yields')+
  theme(
    legend.position=c(0.1, 0.85),
    legend.title = element_blank(),
    legend.box = 1,
    plot.title = element_text( size=15,hjust = 0.5),
    axis.title = element_text( size=15,color = 'Black'),
    text=element_text(size=15,color = 'Black'),
    plot.margin = margin(1,1,1,1,"cm"),
    axis.text = element_text(size=12,color = 'Black')
  )

ggplot(dat%>%
         filter(Fraction=='SEED'),
       aes(y=Biomass,x=Year,group=Treatment))+
  facet_wrap(vars(Species), nrow = 1)+
  geom_point(stat='summary',fun=mean,
    aes(color=Treatment),size=5)+
  geom_errorbar(stat='summary',fun.data=mean_se,
                width =  0.3, size  =  0.5)+
  geom_smooth(method='lm',aes(color=Treatment),se=F)+
  ylab(bquote("Harvested Grain"~g~m^-2))+
  xlab("Year")+
  ggtitle('KBS Crop Yields')+
  theme(
    legend.position=c(0.1, 0.85),
    legend.title = element_blank(),
    legend.box = 1,
    plot.title = element_text( size=15,hjust = 0.5),
    axis.title = element_text( size=15,color = 'Black'),
    text=element_text(size=15,color = 'Black'),
    plot.margin = margin(1,1,1,1,"cm"),
    axis.text = element_text(size=12,color = 'Black')
  )

###

######################################################################
######################## Group Activity ##############################
######################################################################

######################################################################
############ Download, clean and plot your own data ##################
######################################################################

# go to this website https://lter.kbs.msu.edu/datatables
# and find a dataset
# download it, clean it, and plot the data your interested in
# BONUS, analyze the data with a linear model and present the results





