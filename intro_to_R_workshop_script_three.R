### Loading and combining datasets and creating summarised datasets

dat_raw=read.csv(header=T,skip=29,file='https://lter.kbs.msu.edu/datatables/39.csv')
dat=dat_raw[-1,]
dat
head(dat)
str(dat)
dat$Year=as.numeric(dat$Year)
dat=subset(dat,dat$Year>=1993)
dat$Year=as.factor(dat$Year)
dat$Treatment=as.factor(dat$Treatment)
dat$Replicate=as.factor(dat$Replicate)
dat$Station=as.factor(dat$Station)
dat$Species=as.factor(dat$Species)
dat$Fraction=as.factor(dat$Fraction)
str(dat)

length(dat[is.na(dat)])
length(dat[,1][is.na(dat[,1])])
length(dat[,1:8][is.na(dat[,1:8])])
length(dat[,9][is.na(dat[,9])])
dat=na.omit(dat)
unique(dat$Treatment)
dat=subset(dat,Treatment!='T21' & Treatment!='T6')
dat=subset(dat,Species == "Glycine max L. (*)" | Species == "Triticum aestivum L. (*)" | Species == "Zea mays L. (*)" )
dat$Biomass_C=0.45*dat$Biomass
length(dat[is.na(dat)])
unique(dat$Treatment)

library(tidyverse)
library(plotrix)
dat_sum=group_by(dat, Year, Treatment,Species,Fraction) %>% 
  summarise(yield=mean(Biomass_C),yield_se=std.error(Biomass_C))
dat_sum

kbs_precip_map=read.csv(header=T,file="/Users/falvo/Desktop/Dissertation/KBS_Data/KBS_Precip_Yearly_Summary.csv")
kbs_precip_map
kbs_precip_map=kbs_precip_map[2:6]
dat_sum=merge(x=dat_sum,y=kbs_precip_map,by.x='Year',by.y='year')
dat_sum

dat_sum2=group_by(dat, Treatment,Species,Fraction) %>% 
  summarise(yield=mean(Biomass_C),yield_se=std.error(Biomass_C))
dat_sum2

dat_sum2_2=group_by(subset(dat_sum2,Fraction=='WHOLE'), Treatment) %>% 
  summarise(rotation_total_anpp=sum(yield)/3,
            rotation_total_bnpp=0.15*sum(yield)/3,
            rotation_total_npp=1.15*sum(yield)/3)
dat_sum2_2

gg1=ggplot(subset(dat_sum,dat_sum$Fraction=='SEED'),
           aes(y=yield,x=total_cumulative_precip_gs,group=Treatment))+
  geom_point(aes(color=Treatment),size=3)+
  geom_errorbar(aes(ymin  =  yield-yield_se, ymax  =  yield+yield_se),
                width =  0.3, size  =  1, position = position_dodge(0.9))+
  geom_smooth(method='lm',aes(color=Treatment),se=F)+
  ylab("Harvested Grain (g C / sq m)")+
  xlab("Growing Season Precipitation (mm)")+
  theme(
    legend.position=c(0.85, 0.15),
    legend.title = element_blank(),
    legend.box = 1,
    plot.title = element_text( size=15,hjust = 0.5),
    axis.title = element_text( size=15,color = 'Black'),
    text=element_text(size=15,color = 'Black'),
    plot.margin = margin(1,1,1,1,"cm"),
    axis.text = element_text(size=12,color = 'Black')
  )
gg1 + facet_wrap(vars(Species),scales='free', nrow = 2)

gg1_1=ggplot(subset(dat_sum,dat_sum$Fraction=='SEED'),
             aes(y=yield,x=drought,fill=Treatment))+
  geom_boxplot()+
  ylab("Harvested Grain (g C / sq m)")+
  xlab("")+
  theme(
    legend.position=c(0.85, 0.15),
    legend.title = element_blank(),
    legend.box = 1,
    plot.title = element_text( size=15,hjust = 0.5),
    axis.title = element_text( size=15,color = 'Black'),
    text=element_text(size=15,color = 'Black'),
    plot.margin = margin(1,1,1,1,"cm"),
    axis.text = element_text(size=12,color = 'Black')
  )
gg1_1 + facet_wrap(vars(Species),scales='free', nrow = 2)

gg1_2=ggplot(subset(dat_sum,dat_sum$Fraction=='SEED'),
             aes(y=yield,x=storm,fill=Treatment))+
  geom_boxplot()+
  ylab("Harvested Aboveground Biomass (g C/ sq m)")+
  xlab("")+
  theme(
    legend.position=c(0.85, 0.15),
    legend.title = element_blank(),
    legend.box = 1,
    plot.title = element_text( size=15,hjust = 0.5),
    axis.title = element_text( size=15,color = 'Black'),
    text=element_text(size=15,color = 'Black'),
    plot.margin = margin(1,1,1,1,"cm"),
    axis.text = element_text(size=12,color = 'Black')
  )
gg1_2 + facet_wrap(vars(Species),scales='free', nrow = 2)

gg2=ggplot(subset(dat_sum2,dat_sum2$Fraction=='WHOLE'),
           aes(y=yield,x=Treatment,group=Species))+
  geom_bar(stat="identity", position=position_dodge(), show.legend = F,
           aes(fill=Treatment))+
  geom_errorbar(aes(ymin  =  yield-yield_se, ymax  =  yield+yield_se),
                width =  0.3, size  =  1, position = position_dodge(0.9))+
  xlab("")+
  ggtitle("KBS LTER MCSE Yields\n")+
  theme(
    legend.position=c(0.15, 0.9),
    legend.title = element_blank(),
    legend.box = 1,
    plot.title = element_text( size=15,hjust = 0.5),
    axis.title = element_text( size=15,color = 'Black'),
    text=element_text(size=15,color = 'Black'),
    plot.margin = margin(1,1,1,1,"cm"),
    axis.text = element_text(size=12,color = 'Black')
  )
gg2 + facet_wrap(vars(Species), nrow = 2)

###
