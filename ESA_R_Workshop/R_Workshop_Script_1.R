### Clear environment
rm(list = ls())
invisible(lapply(paste0("package:", names(sessionInfo()$otherPkgs)), detach,character.only = T, unload = T))

### load packages
library(tidyverse)
library(lmerTest)
library(emmeans)
library(sjPlot)
library(gt)

### set ggplot theme
theme_set(theme_classic()) 
custom_theme=  theme(
  plot.title = element_text( size=15,hjust = 0.5,color = 'Black'),
  axis.title = element_text( size=15,color = 'Black'),
  text=element_text(size=15,color = 'Black'),
  axis.text = element_text(size=15,color = 'Black'))

### set working directory
### change path to your downloaded R workshop folder
### use full path name
setwd(
  '/Users/falvo/Desktop/Dissertation/intro_to_R_workshop/ESA_R_Workshop'
  )


#### 
# Bill, K.E., C.M. Dieleman, J.L. Baltzer, G.É. Degré-Timmons, 
# M.C. Mack, N.J. Day, S.G. Cumming, X.J. Walker, and M.R. Turetsky. 
# 2023. Post-fire Recovery of Soil Organic Layer Carbon in Canadian 
# Boreal Forests. Ecosystems. https://doi.org/10.1007/s10021-023-00854-0
####

### set variable holding the online location of the data
data_url='https://daac.ornl.gov/daacdata/above/Post_Fire_SOC_NWT/data/SoilOrganicCarbonRecovery_NorthwestTerritories.csv'

### download the data to your working directory
### -c argument checks for identical local file, if present, doesn't redownload
system(paste0('wget -c ',data_url)) 

### load the data into R and look at its structure
dat=read.csv(
  'SoilOrganicCarbonRecovery_NorthwestTerritories.csv'
)
str(dat)

## Reproduce Figure 2 from manuscript
ggplot(dat%>%
         mutate(ecozone=recode(ecozone,
                               plains='Tiaga Plains',
                               shield='Tiaga Shield')),
       aes(x=time_after_fire,
           y=avg_org_carbon
           ))+
  custom_theme+
  theme(legend.position="bottom",
        strip.background = element_blank())+
  ylab('SOL Thickness (cm)')+
  xlab('Time-after-fire (yrs)')+
  facet_wrap(vars(ecozone))+
  geom_point(aes(fill=moisture,shape=moisture),
             color='black',size=3,show.legend = T)+
  scale_shape_manual(values=c(21,22,24))+
  geom_smooth(aes(color=moisture,
                  fill=moisture),
              se=T,show.legend = F,
              method='lm',alpha=0.3
              )+
  guides(fill=guide_legend(title="Moisture Class"),
         shape=guide_legend(title="Moisture Class"),
         color=guide_legend(title="Moisture Class"))+
  scale_fill_manual(values=c('darkblue','steelblue','grey'))+
  scale_color_manual(values=c('darkblue','steelblue','grey'))

### 2  incomplete observations complicate subsequent steps
### and are removed here
dat=dat%>%
  filter(!if_any(site:ecozone, is.na))

### construct the linear mixed effect model used in the manuscript
datlm=lmer(log(avg_org_carbon)~
             time_after_fire*ecozone*moisture+
             stand_dominance+
             nonvascular_functionalgroup+
             decid_prop_canopy+
             piba_prop_canopy+
             (1|site),
           data=dat
             )

### look at model diagnostics
summary(datlm)
joint_tests(datlm)
plot_model(datlm,type='diag')

### create dataset of marginal means from model
datlm_emmeans=emmeans(datlm,
                      ~ecozone*moisture|time_after_fire,
                      #at=list(time_after_fire=1:100),
                      type='response',
                      rg.limit=20000
                      )%>%
  multcomp::cld(Letters=letters,adjust='none')%>%
  mutate(.group=trimws(.group))%>%
  mutate(across(where(is.numeric), round, 1))
datlm_emmeans

### create table from marginal means dataframe
datlm_emmeans%>%
  gt(  rowname_col = "ecozone",
       groupname_col = "moisture",
       row_group_as_column = T) #%>%
  #gtsave(filename = "Table_1.docx")

### create figure showing statistical results
figure_1=ggplot(datlm_emmeans,
                aes(x=moisture,
                    y=response))+
  custom_theme+
  xlab(NULL)+
  theme(legend.position = c(0.85,0.85))+
  coord_cartesian(ylim=c(0,40))+
  ylab('SOL Thickness (cm)')+
  xlab('Moisture Class')+
  geom_hline(yintercept = 0)+
  geom_bar(stat='identity',aes(fill=ecozone),color='black',
           show.legend = T,width=0.75,position=position_dodge(0.9))+
  scale_fill_manual(values=c('black','darkgrey','lightgrey'))+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL,
                    group=ecozone),width=0.25,
                position=position_dodge(0.9))+
  geom_text(aes(y=upper.CL+3,label=.group,group=ecozone),size=7,
            position=position_dodge(0.9))
figure_1

# ggsave(plot=figure_1,
#        filename='Figure_1.png',
#        units='cm',width=15,height=15,
# )


