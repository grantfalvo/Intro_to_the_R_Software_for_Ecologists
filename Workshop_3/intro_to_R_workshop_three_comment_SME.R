### clear your environment
rm(list = ls())
### unload user installed packages 
.rs.restartR()

### Load (and install, if needed) Packages
library(tidyverse)

### Set working directory


### load extra-floral nectary color data (trial dataset)


# dat <- read.csv("EFNdata.csv")

############################### REVIEW #################################

### 1. look at the data
  # based on what you see, what kind of data is in this dataframe?


### 2. Print the barcode column 


### 3. Rename the "color" column to be "efnColor"


### 4. Make a new column called "prop" that is the proportion of EFNs infected by mold


### 5. Make a new dataframe called "dat2" that is just the species and color columns



########## LINEAR MODEL REVIEW #############

### 5. Run a linear model (using `lm()`) examining the effect of rainfall on EFN fungal infection. Save it as an object, and look at the summary. What have you learned about rainfall's effect on EFN infection?


############################### NEW MATERIAL #################################

  # first, let's go look at the R Graph Gallery. So much potential! 
# https://r-graph-gallery.com/

### Histogram: useful for looking at the distribution of a vector
  # make a histogram of proportion infected


# hist(dat$prop)
  # what is the range of the data? 

### Scatter plot: compare two continuous values
                  ## The formula for base R plots is *** y ~ x *** 
## here, we ask "how does the number of infected EFNs vary with the total EFNs present?"


# plot(infected~total, data = dat)

## We can change the plotted shape 


# plot(infected~total, pch = 16)

## Challenge 1: Make a plot of how rainfall impacts infection rate. 




### Boxplot: compare continuous values between some categories
## here, we will look at EFN infection rate between different species


# boxplot(prop~species, data = dat)
  
  ### Now let's change the color of the bars



# boxplot(prop~species, col = "red")

  ### We can also change the y label to "proportion of EFNs infected"



# boxplot(prop~species, col = "red", ylab = "proportion of EFNs infected")

  ## Challenge 2. Make a boxplot of the rainfall each species experiences. 





### Brief point: plot vs boxplot and understanding errors



# plot(prop ~ species, data = dat) # throws error "need finite 'xlim' values"
# boxplot(prop ~ rainfall, data = dat) # runs but is uninterpretable

######################### INTRO TO GGPLOT #########################
 ### ggplot works very differently than base R. Both are useful tools. 
    ## Being familiar with both lets you
      # 1. Work with different collaborators and mentors
      # 2. Use the right tool for the job
      # 3. Choose the syntax that works best for your brain

### Basic scatterplot (equivalent of plot())


# ggplot(data = dat, aes(x = total, y = infected)) + geom_point()

### Basic boxplot 



# dat %>% 
# ggplot(aes(x = species, y = prop)) + 
#   geom_boxplot() 

### we can also overlay the data points onto our boxplot


# dat %>%
# ggplot(aes(x = species, y = prop)) + 
#   geom_boxplot() + 
#   geom_point()


######## 19 minutes to here ######### 

### Challenge 3: Make a graph with of color vs prop; make a base R and a ggplot version. Change the y axis label in both plots to ("Proportion of EFNs Infected")




### Using color to add additional data ###

### Color the boxes of our species vs proportion infected plot red



### Color the boxes of the boxplot by EFN color (categorical variable)



# dat %>%
# ggplot(aes(x = species, y = prop, fill = efnColor)) + 
#   geom_boxplot() + 
#   geom_point()
  

### Pick some fun fancy colors 



`# What can we now say about the influence of EFN color on infection?

# dat %>%
# ggplot(aes(x = species, y = prop, fill = efnColor)) + 
#   geom_boxplot() + 
#   scale_fill_manual(values = c("olivedrab", "tomato"))

### Learning about global vs local aesthetics 




# dat %>%
# ggplot(aes(x = species, y = prop, color = efnColor)) + 
#   geom_boxplot(color = "green") + 
#   geom_point(color = "black") + 
#   theme_bw()

############## 12 mins for color section ###########

## Challenge 4: Make a plot with either base R or ggplot using this EFN dataset; decide which variables to plot, and what kind of plot you'll need. Once you have your base plot, tweak the color, point shape, and/or axis labels. If you finish quickly, google how to change another aspect of the plot (font size, background color, the range of the y and x axis values, etc), and try it on your plot. 




#################### Saving plots #################### (7.5 mins)

## Working directories: the folder of your computer where R is looking
getwd()

## Let's take a moment to talk about file organization


## start by setting your working directory to the folder where you want your plot to appear


  # open a .png file


# png("samplePlot.png", width = 500, height = 500, res = 2000)

  # run your plot code

# dat %>%
# ggplot(aes(x = species, y = prop, fill = efnColor)) + 
#   geom_boxplot() + 
#   scale_fill_manual(values = c("olivedrab", "tomato"))


  # close the file


# dev.off()


##### Alternate method with 'ggsave()' #####

### print your plot (either ggplot or baseR code)


# dat %>%
# ggplot(aes(x = species, y = prop, fill = efnColor)) + 
#   geom_boxplot() + 
#   scale_fill_manual(values = c("olivedrab", "tomato"))


### use ggsave; this will save the last plot you ran


# ggsave("sampleggsave.png")


#################### Bar plot ###################

# Traditionally, bar plots are created by making a summary dataframe that contains the means and standard errors of each group. However, ggplot can conveniently calculate this for you with a few extra arguments!



# dat %>%
# ggplot(aes(x = species, y = prop)) + 
#   geom_bar(stat = "summary") + 
#   geom_errorbar(stat = "summary", width = 0.2)

#################### FORMATING 2.0 #################### (2.5 min)

# There are lots of fine details that ggplot can do. It's always ok to use powerpoint to fiddle with the small details later, but here are some common additional functions. 
# ggplot() has an immense amount of online forum and tutorial pages dedicated to explaining its detailed functions, making it easy to learn on your own. 

### change the position of the legend or remove the legend



# dat %>%
# ggplot(aes(x = species, y = prop, fill = efnColor)) +
#   geom_boxplot() +
#   theme(legend.position = "top") # or legend.position = "none"


### add a title; + labs(title = "EFN color and infection rate")


# dat %>%
# ggplot(aes(x = species, y = prop, fill = efnColor)) + 
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


