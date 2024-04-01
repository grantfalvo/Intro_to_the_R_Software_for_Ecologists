### Clear environment and unload packages
rm(list = ls())
invisible(lapply(paste0("package:", names(sessionInfo()$otherPkgs)), detach,character.only = T, unload = T))

### load packages
### install if necessary with install.packages('PACKAGE_NAME_HERE')
library(tidyverse)
library(httr)
library(jsonlite)
library(foreach)
library(doParallel)
library(lmerTest)
library(emmeans)
library(sjPlot)
library(lubridate)
library(gt)
library(scales)

### set ggplot theme
theme_set(theme_classic()) 
custom_theme=  theme(
  legend.title = element_blank(),
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

### MODIS API explorer website
'https://modis.ornl.gov/rst/ui/#!/products/get_products'

### set variable holding the online location of the data
data_url <- "https://modis.ornl.gov/rst/api/v1/"

### testing the API by looking for available products
request='products?sensor=MODIS-Terra'
paste0(data_url,request)
GET(paste0(data_url,request))
response=data.frame(fromJSON(paste0(data_url,request)))
view(response)

### we'll be using the yearly NPP product called MOD17A3HGF
### https://lpdaac.usgs.gov/documents/972/MOD17_User_Guide_V61.pdf
### https://lpdaac.usgs.gov/products/mod17a3hgfv006/
### define API parameters for this product
product = 'MOD17A3HGF' # MODIS annual NPP product 
data_band ='Npp_500m' # band name
qc_band='Npp_QC_500m' # band name
above_below = 0 # km above/below # set to 0 for point extraction
left_right = 0 # km left/right # set to 0 for point extraction
chunk_size=365*10 # 365 day product, 10 date API limit

### this function generates the URL for the API request we make
generate_request <- function(latitude, longitude, band,
                             start_date, end_date, kmAB, kmLR){
  return(
    paste0(
      data_url,product,"/subset?",
      "latitude=",latitude,
      "&longitude=",longitude,
      "&band=",band,
      "&startDate=",start_date,
      "&endDate=",end_date,
      "&kmAboveBelow=",kmAB,
      "&kmLeftRight=",kmLR
    )
  )
}

### this function turns R formatted dates into a format that the API wants
create_modis_date=function(date){
  paste0('A',year(date),sprintf("%03d", yday(date)))
}

### this function creates the date chunks for our request 
get_date_chunks=function(start_date,end_date,chunk_size){
  temp_dat=data.frame(date=seq.Date(start_date,end_date,1),
             date_chunk_groups=((seq.Date(start_date,end_date,1)-(start_date+chunk_size))/chunk_size)%>%
               floor()%>%
               as.character()
  )
  temp_dat%>%
    group_by(date_chunk_groups)%>%
    summarise(start_date=min(date),
              end_date=max(date))
}

### this function downloads the NPP timeseries and QC information for one site
get_site_timeseries=function(site_name,lat,lon,start_date,end_date){
  date_chunks=get_date_chunks(start_date,end_date,chunk_size)
  
  site_dat=foreach(i=1:length(date_chunks$date_chunk_groups),.combine=rbind)%do%{
    data_request=generate_request(
      lat,lon,data_band,
      create_modis_date(date_chunks$start_date[i]),
      create_modis_date(date_chunks$end_date[i]),
      above_below,left_right) 
    data_response=fromJSON(data_request)$subset%>%
      data.frame()%>%
      unnest(cols=data)%>%
      mutate(npp=data,
            data=NULL,
            band=NULL)
    qc_request=generate_request(
      lat,lon,qc_band,
      create_modis_date(date_chunks$start_date[i]),
      create_modis_date(date_chunks$end_date[i]),
      above_below,left_right) 
    qc_response=fromJSON(qc_request)$subset%>%
      data.frame()%>%
      unnest(cols=data)%>%
      mutate(npp_qc=data,
            data=NULL,
            band=NULL)
  
    temp_dat=merge(data_response,qc_response)

    temp_dat=temp_dat%>%
      mutate(
         site_name=site_name,
         lat=lat,
         lon=lon,
         date=as.Date(calendar_date),
         npp=ifelse(npp>32700|npp<1,NA,npp), ### remove fill values for bad pixels
         npp=npp*0.1, #g C per sq m per y
         npp_qc=npp_qc*0.01 # % of data that is gapfilled
         )
    return(temp_dat)
  }
  return(site_dat)
}

######################################################################

### this dataframe defines the sites and dates you want data for
### you can add variables to describe the site details such as
### vegetation type, replicate_id, years since disturbance, etc. 
site_dat=data.frame(
  site_name=c('Atacama Foothills',
              'Amazon Forest Intact',
              'Amazon Forest Cleared'
              ),
  
  lat=c(-16.413018,
        -9.268234,
        -10.398518
        ),
  
  lon=c(-71.346923,
        -54.117681,
        -55.289391
        ),
  
  start_date=c(
    '2003-01-01',
    '2003-01-01',
    '2003-01-01'
  ),
  
  end_date=c(
    '2023-01-01',
    '2023-01-01',
    '2023-01-01'
  )
)

### this initializes the multicore parallization scheme
cl <- makeCluster(3) # depends on how many cores your computer has
registerDoParallel(cl)

### this code loops through the site dataframe you created
### and downloads the data from the API in parallel
### system.time wrapper times the download process
system.time({
dat=foreach(i=1:length(site_dat$site_name),.combine=rbind,
            .packages = c('tidyverse','foreach','jsonlite','httr')
            )%dopar%{
  temp_dat=get_site_timeseries(site_name=site_dat$site_name[i],
                               lat=site_dat$lat[i],
                               lon=site_dat$lon[i],
                    start_date=as.Date(site_dat$start_date[i]), # dates of interest for all sites
                    end_date=as.Date(site_dat$end_date[i]))  # dates of interest for all sites
  temp_dat=merge(temp_dat,site_dat,all.x=T)
  return(temp_dat)
            }
})
### remember to shut down the multicore scheme
stopCluster(cl)

### inspect your data 
str(dat)

### inspect your data visually
ggplot(dat,
       aes(x=date,
           y=npp))+
  custom_theme+
  ylab(bquote(NPP~g~C~m^-2~y^-1))+
  xlab('Year')+
  geom_hline(yintercept = 0)+
  coord_cartesian(ylim=c(0,2000))+
  geom_point(
    aes(color=site_name)
  )

############################ Coding Challenge ##############################

# your challenge is to come up with a research question that can be answered
# with annual NPP data. You will write/modify code to acquire that data, 
# analyze it statistically and produce one table and one figure that can  
# be used in a publication answering your research question

# modify the site names, locations, dates and other variables 
# in the 'site_dat' dataframe to select sites of interest to you. 
# Select sites that will allow you to use annual NPP to answer a 
# research question. This can be a question about plant productivity 
# across space and/or time in response to natural/anthropogenic disturbances 
# and/or natural variability
#
# In order to run statistical tests with a linear regression,
# make sure that you have more than one kind of site and more than one
# replicate within each site. For example, 3 forest sites and 3 grassland
# sites for a total of 6 sites. You will need to add a replicate_id variable
# in the 'site_dat' dataframe to identify your replicates.

# Once downloaded, inspect your dataset to ensure it is what you expect
# Once satisfied, construct your linear regression model and check that 
# your model meets its assumptions
# Conduct and F-test (a.k.a ANOVA) to answer your research question

# generate 1 table with and 1 figure that answer your research question
# save your raw data, table and figure in a directory for 
# use in your publication

############################ Coding Challenge ##############################

write.csv(dat,row.names = F,
          'raw_npp_data.csv'
          )

str(dat)

datlm=lmer(npp~site_name+
             (1|date),
         data=dat)

summary(datlm)
joint_tests(datlm)
plot_model(datlm,'diag')

datlm_emmeans=emmeans(datlm,~site_name)%>%
  multcomp::cld(Letters=letters,adjust='none')%>%
  mutate(.group=trimws(.group))%>%
  mutate(across(where(is.numeric), round, 0))
datlm_emmeans

datlm_emmeans%>%
  gt()%>%
  gtsave(filename = "Table_1.docx")

figure_1=ggplot(datlm_emmeans,
       aes(x=site_name,
           y=emmean))+
  custom_theme+
  xlab(NULL)+
  scale_x_discrete(labels = label_wrap(10))+
  coord_cartesian(ylim=c(0,2000))+
  ylab(bquote(NPP~g~C~m^-2~y^-1))+
  geom_hline(yintercept = 0)+
  geom_bar(stat='identity',aes(fill=site_name),color='black',
           show.legend = F,width=0.75)+
  scale_fill_manual(values=c('black','darkgrey','lightgrey'))+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=0.25)+
  geom_text(aes(y=upper.CL,label=.group),size=7,nudge_y=100)
figure_1

ggsave(plot=figure_1,
       filename='Figure_1.png',
       units='cm',width=15,height=15,
       )

############################ Coding Challenge ##############################

# Start your code here
# be careful and deliberate about copying code and reusing names/variables 
# already defined above in this script






