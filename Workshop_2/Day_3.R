### clear your environment
rm(list = ls())
### unload user installed packages 
invisible(lapply(paste0("package:", names(sessionInfo()$otherPkgs)), detach,character.only = TRUE, unload = TRUE))

library(tidyverse)
dat_raw=read.csv(header=T,skip=29,file='https://lter.kbs.msu.edu/datatables/39.csv') # loads the KBS data set as a data frame
dat=dat_raw[-1,] # deletes the first row (empty entry) from the data frame
dat       # print in the console the first 112 rows of dat
head(dat) # print in the console the first 6 rows of dat
str(dat)  # shows all the variables (columns) in dat, what kind of variable they are, and the first 4 entries.
          # note that year is currently a character variable
dat$Year=as.numeric(dat$Year)  # transforms the variable Year into a numeric variable
dat=subset(dat,dat$Year>=1993) # subset the data frame selecting only observations (rows) from 1993 on. 
dat$Treatment=as.factor(dat$Treatment) # transforms the variable Treatment into a factor variable 
dat$Replicate=as.factor(dat$Replicate)
dat$Station=as.factor(dat$Station)
dat$Species=as.factor(dat$Species)
dat$Fraction=as.factor(dat$Fraction)
str(dat)

dat=na.omit(dat) # erases from the data frame all NA observations (full rows)
unique(dat$Treatment)  # Prints in the console the different 'levels' of the factor variable Treatment.
dat=dat%>%            
  filter(Treatment!='T21' & Treatment!='T6')%>%   # select observations from all treatments besides treatments 21 and 6
  filter(Species == "Glycine max L. (*)" |        # select observations of the species: Glycine max L. (*), Triticum aestivum L. (*), "Zea mays L. (*)" 
           Species == "Triticum aestivum L. (*)" | 
           Species == "Zea mays L. (*)" )%>%
  filter(!is.na(Biomass))
dat$Species <- recode_factor(dat$Species,        
                             'Glycine max L. (*)' = "Soybean", 
                             "Triticum aestivum L. (*)" = "Wheat",
                             "Zea mays L. (*)" = 'Corn')              # renames the species 
library(plotrix)
dat_sum=group_by(dat, Year, Treatment,Species,Fraction) %>%  # groups the observations in dat with the same values of year, treatment, species, and fraction
  summarise(yield=mean(Biomass),yield_se=std.error(Biomass)) # creates two new variables, yield and yield_se that are the mean and standard error Biomass respectively
dat_sum 

########### PLOTTING OUR DATA ########################

# HISTOGRAM #

hist(dat$Biomass)            # makes simple HISTOGRAM of Biomass (base R)

ggplot(dat,aes(x=Biomass))+  # "tells ggplot" what is the dataset and the variables to be plotted
  geom_histogram()           # tells ggplot what kind of plot we want (histogram)

# BOX PLOT #                             

boxplot(Biomass~Species,dat)            # makes simple BOX PLOT of Biomass ~ Species (base R)

ggplot(dat,aes(x=Species,y=Biomass))+   # tells ggplot what is the dataset and the variables to be plotted
  geom_boxplot()                        # tells ggplot what kind of plot we want (box plot)

# BAR PLOT #

barplot(yield~Year,dat_sum %>%          
          filter(Species=='Corn' & Treatment=='T1' & Fraction=='SEED')) # makes simple BAR PLOT of Yield ~ Year but only for Corn, in Treatment 1, and fraction = seed (base R)

ggplot(dat_sum %>% 
         filter(Species=='Corn' & Treatment=='T1' & Fraction=='SEED'),  # tells ggplot what is the dataset (dat_sum, filtered)
       aes(x=Year,y=yield))+                                            # tells ggplot what is the dataset and the variables to be plotted
  geom_bar(stat='identity')   # Why do we need "stat = 'identity'"? Try googling the following to find out: "geom_bar(stat='identity')"


# SCATTER PLOT #

plot(Biomass~Year,dat)

ggplot(dat,aes(x=Year,y=Biomass))+
  geom_point()

########### IMPROVING OUR PLOTS IN GGPLOT ########################

# Note that until now we have been only making very simple plots, with no title, colors, or legends.
# You can add and change elements to your plots in both base R and ggplot. 
# Here we will show you a few of these possibilities in ggplot. 
# For R base tutorials you can go here: https://r-coder.com/plot-r/ or check other endless online resources


# Let's plot the yield of each species by year, grouping observations by treatment.
ggplot(dat_sum%>%
         filter(Fraction=='SEED'),
       aes(y=yield,x=Year,group=Treatment))+   # Tells ggplot what are the x and y variables
  facet_wrap(vars(Species), nrow = 1)+         # Specify that we want one plot for each species, plotted in 1 row
  geom_point(aes(color=Treatment),size=5)+     # determines we want a scatter plot, each treatment to have a different color, and sets the point size to 5
  geom_errorbar(aes(ymin  =  yield-yield_se, ymax  =  yield+yield_se),  # includes the error bars for each point (using the standard error that we calculated above)
                width =  0.3, size  =  0.5)+                            # determines the size and width of the error bars
  geom_smooth(method='lm',aes(color=Treatment),se=F)+  # Adds the trend line to each plot, using a linear model method (lm), a different color for each treatment, without the confidence interval around the line (se = FALSE)
  ylab(bquote("Harvested Grain"~g~m^-2))+              # Adds the Y axis title
  xlab("Year")+                                        # Adds the X axis title
  ggtitle('KBS Crop Yields')+                          # Adds a general plot title
  theme(                                                
    legend.position=c(0.1, 0.85),                      # specify legend position
    legend.title = element_blank(),                    # specify legend title (none)
    legend.box = 1,                                    
    plot.title = element_text( size=15,hjust = 0.5),   # Set size and justification of title (The value of hjust is only defined between 0 and 1: 0 means left-justified and 1 means right-justified)
    axis.title = element_text( size=15,color = 'Black'),
    text=element_text(size=15,color = 'Black'),
    plot.margin = margin(1,1,1,1,"cm"),                # Set the size of plot margins
    axis.text = element_text(size=12,color = 'Black')  # Set the size of axis text
  )

# You can try changing up values in the code and see how that changes your plot. 

# Do you notice the difference between this code and the code above? 
# this is a way to let R summarize your data using a specific function (in this case the mean) in your plot
# To start learning about all the possible arguments of geom-point check this : https://ggplot2.tidyverse.org/reference/geom_point.html or try googling geom_point() r 

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


########### BUT WE CAN DO SO MUCH MORE ########################


# Lets make a map and plot the places we have been (East Lansing and KBS)

#install.packages('rgeos', type='source')
#install.packages('rgdal', type='source')
#install.packages('broom')
#install.packages('maptools')

library(rgdal)
library(rgeos)
library(maptools)
library(broom)

download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="./world_shape_file.zip")
system("unzip ./world_shape_file.zip")  #downloads a shapefile pf the world

my_spdf <- readOGR( "./TM_WORLD_BORDERS_SIMPL-0.3.shp" ,
  verbose=FALSE
)   # reads the shapefile 


summary(my_spdf) # tells you the max and min coordinates, the kind of projection in use
length(my_spdf) # how many regions you have
head(my_spdf@data) # the first few rows of the data slot associated with the regions

# Basic plot of this shape file:
par(mar=c(0,0,0,0))                              # sets the margins
plot(my_spdf, col="#f2f2f2", bg="skyblue", lwd=0.25, border=0 )

# ggplot 

# To plot our data using ggplot, we will need to transform our data into a data frame. 

spdf_fortified <- tidy(my_spdf, region = "NAME")

# Plot it
library(ggplot2)

cities <- data.frame(ID = c("East Lansing","KBS"),
                     x = c(42.736980, 42.405093),
                     y = c(-84.483864, -85.400665))   # creates a data frame for the cities we visited, using latitude and longitude data

ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill="darkgreen", color="white") +
  geom_point(data = cities, aes(x = y, y = x),col="red", size=1) + # add the cities
theme_void()


# Now let's make an animated plot using our initial data! 


# install.packages("gganimate")
# install.packages("gifski")
library(gifski)
library(ggplot2)
library(gganimate)


# We will plot the yield of Wheat (fraction = seed), for the different treatments through the years. 
# Note how you are familiar with most of this code already. 

p<-ggplot(dat_sum%>%
            filter(Fraction=='SEED' & Species == 'Wheat'), 
          aes(x=Treatment, y=yield, fill=as.factor(Treatment))) + 
  geom_bar(stat='identity') +
  theme_bw() +
  # gganimate specific bits:
  transition_states(
    Year,                   # Tells ggplot what is the variable for different frames. 
    transition_length = 2,
    state_length = 1
  ) +
  ease_aes('sine-in-out') +
  labs(title  = 'Year: {closest_state}')


animate(p, renderer = gifski_renderer())


###

# For more R graphs tutorials and inspiration check the R GRAPH GALLERY: https://r-graph-gallery.com/

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









