### clear your environment
rm(list = ls())
### unload user installed packages 
.rs.restartR()

### Load (and install, if needed) Packages
library(tidyverse)

### load extra-floral nectary color data (trial dataset)
# dat <- read.csv("EFNdata.csv")

############################### REVIEW #################################

### 1. look at the data
  # based on what you see, what kind of data is in this dataframe?


### 2. Print the barcode column 


### 3. Make a new column called "propInfected" that is the proportion of EFNs infected by mold


### 4. Make a new dataframe called "efnCol" that is just the species and color columns



########## LINEAR MODEL REVIEW #############

### 5. Run a linear model (using `lm()`) examining the effect of rainfall on EFN fungal infection. Save it as an object, and look at the summary. What have you learned about rainfall's effect on EFN infection?


############################### NEW MATERIAL #################################

  # first, let's go look at the R Graph Gallery. So much potential! 
# https://r-graph-gallery.com/

### Histogram: useful for looking at the distribution of a vector
  # make a histogram of proportion infected
# hist(dat$propInfected)
  # what is the range of the data? 

### Scatter plot: compare two continuous values
## here, we ask "how does the number of infected EFNs vary with the total EFNs present?"
# plot(infected~total, data = dat)

## We can change the plotted shape 
# plot(infected~total, data = dat, pch = 16)

## Challenge 1: Make a plot of how rainfall impacts infection rate. 


### Boxplot: compare continuous values between some categories
## here, we will look at EFN infection *rate* between different species
  ## The formula for base R plots is *** y ~ x *** 
# boxplot(propInfected~species, data = dat)
  
  # Now let's change the color of the bars
# boxplot(propInfected~species, data = dat, col = "red")

  # We can also change the y label to "proportion of EFNs infected"
# boxplot(propInfected~species, data = dat, col = "red", ylab = "proportion of EFNs infected")

  ## Challenge 2. Make a boxplot of the rainfall each species experiences. 


######################### INTRO TO GGPLOT #########################
 ### ggplot works very differently than base R. Both are useful tools. 
    ## Being familiar with both lets you
      # 1. Work with different collaborators and mentors
      # 2. Use the right tool for the job
      # 3. Choose the syntax that works best for your brain

# Basic scatterplot (equivalent of plot())
# ggplot(data = dat, aes(x = total, y = infected)) + geom_point()

# Basic boxplot (note how we can write ggplot code on multiple lines)
# ggplot(data = dat, aes(x = species, y = propInfected)) + 
#   geom_boxplot() 

    # we can also overlay the data points onto our boxplot
# ggplot(data = dat, aes(x = species, y = propInfected)) + 
#   geom_boxplot() + 
#   geom_point()

  # Try changing the order (data argument vs geom_boxplot() and geom_point())
# ggplot(data = dat, aes(x = species, y = propInfected)) + 
#   geom_point() + 
#   geom_boxplot()

### Challenge 3: Make a graph with of color vs propInfected; make a base R and a ggplot version. Change the y axis label in both plots to ("Proportion of EFNs Infected")


### Using color to add additional data ###

# Color the boxes of our rainfall vs proportion infected plot red
# ggplot(data = dat, aes(x = rainfall, y = propInfected)) + 
#   geom_point(color = "red")

# Color the points by EFN color ("color" column)
# ggplot(data = dat, aes(x = rainfall, y = propInfected, color = color)) + 
#   geom_point()

# Color the boxes of the boxplot red
# ggplot(data = dat, aes(x = species, y = propInfected)) + 
#   geom_boxplot(fill = "red") 

# Color the boxes of the boxplot by EFN color (categorical variable)
# ggplot(data = dat, aes(x = species, y = propInfected, fill = color)) + 
#   geom_boxplot() + 
#   geom_point()
  # What can we now say about the influence of EFN color on infection?

# Pick some fun fancy colors 
# ggplot(data = dat, aes(x = species, y = propInfected, fill = color)) + 
#   geom_boxplot() + 
#   scale_fill_manual(values = c("olivedrab", "tomato"))

# Learning about global vs local aesthetics 
# ggplot(data = dat, aes(x = species, y = propInfected, color = color)) + 
#   geom_boxplot(color = "green") + 
#   geom_point(color = "black") + 
#   theme_bw()

## Challenge 4: Make a plot with either base R or ggplot using this EFN dataset; decide which variables to plot, and what kind of plot you'll need. Once you have your base plot, tweak the color, point shape, and/or axis labels. If you finish quickly, google how to change another aspect of the plot (font size, background color, the range of the y and x axis values, etc), and try it on your plot. 



#################### Saving plots ####################

## Working directories: the folder of your computer where R is looking
# getwd()

## Let's take a moment to talk about file organization 

## start by setting your working directory to the folder where you want your plot to appear

## save your plot
  # open a .png file
# png("testplot.png", width = 500, height = 500, res = 2000)

  # run your plot code
# ggplot(data = dat, aes(x = species, y = propInfected, fill = color)) + 
#   geom_boxplot() + 
#   scale_fill_manual(values = c("olivedrab", "tomato"))

  # close the file
# dev.off()

## Alternate method with 'ggsave()'

# print your plot (either ggplot or baseR code)
# ggplot(data = dat, aes(x = species, y = propInfected, fill = color)) + 
#   geom_boxplot() + 
#   scale_fill_manual(values = c("olivedrab", "tomato"))

# use ggsave; this will save the last plot you ran
# ggsave("testggsave.png")
  
# change the height and width; save as a new file name
# ggsave("testggsave2.png", height = 2, width = 4, units = "in")

#################### Bar plot ###################

# Traditionally, bar plots are created by making a summary dataframe that contains the means and standard errors of each group. However, ggplot can conveniently calculate this for you with a few extra arguments!
# ggplot(data = dat, aes(x = species, y = propInfected, fill = color)) + 
#   geom_bar(stat = "summary") + 
#   geom_errorbar(stat = "summary", width = 0.2)

#################### FORMATING 2.0 ####################

# There are lots of fine details that ggplot can do. It's always ok to use powerpoint to fiddle with the small details later, but here are some common additional functions. ggplot() has an immense amount of online forum and tutorial pages dedicated to explaining its detailed functions, making it easy to learn on your own. 

# change the position of the legend 
# ggplot(data = dat, aes(x = species, y = propInfected, fill = color)) + 
#   geom_boxplot() + 
#   theme(legend.position = "top")

# remove the legend (legend.position = "none")
# ggplot(data = dat, aes(x = species, y = propInfected, fill = color)) + 
#   geom_boxplot() + 
#   theme(legend.position = "none")

# add a title; + labs(title = "EFN color and infection rate")
# ggplot(data = dat, aes(x = species, y = propInfected, fill = color)) + 
#   geom_boxplot() + 
#   labs(title = "EFN color and infection rate")


######################################################################
######################## Group Activity ##############################
######################################################################

######################################################################
######### Read in, clean, plot and analyze your own data ############
######################################################################

# Using data from your summer work or your lab (or the data provided), clean and plot the data you're interested in
# Remember the R graph gallery has lots of example code for different kinds of plots
# Save your graph in the folder where you keep your KBS R scripts. 
# If you do not have your own data yet, that's fine! Use the attached dataset from Sylvie's first chapter. Ask Sylvie for help understanding the experimental design and specifics of the project. Check the metadata tab : ). 


# Bonus: Find a comparison you can analyze with a linear regression model and plot the results. Save your model as an object and practice interpreting the model output. 


