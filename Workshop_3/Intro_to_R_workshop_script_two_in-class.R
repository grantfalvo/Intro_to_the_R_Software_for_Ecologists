# Welcome to Part 2 of Intro to R Workshop

# Setup--------------------------------------------------------------------------
rm(list = ls()) # clear your environment
invisible(lapply(paste0("package:", names(sessionInfo()$otherPkgs)), detach,character.only = TRUE, unload = TRUE)) #unload user installed packages
#setwd(" ") #change working directory to new location

# Review from Part 1--------------------------------------------------------------
# What is 56.7 x 123.4, rounded to 1 digit?


# What is the mean of 2.3, 4.5, 6.7, and 3.9?


# In the following data frame:
df = data.frame(numbers = c(1,2,3),letters = c("a","b","c"))

# check the data type of the 2 columns

# change the second column to a factor data type

# return the 1st row, 2nd column

# return only the row in which the numbers column is >1 and the letters column does not equal "b"


# Clean data -------------------------------------------------------
install.packages("lubridate") #install package for working with date data
library(lubridate) #load the package

###load in KBS weather station data
dat_raw=read.csv(file="https://lter.kbs.msu.edu/datatables/12.csv",skip=44,header = T) #read the data directly from the KBS website, skipping the 1st 44 rows that don't contain data
#for descriptions of variables, see: https://lter.kbs.msu.edu/datatables/12


### change the data type of columns that are incorrect


### create a clean dataset with only relevant columns


### plot each variable to look for outliers
install.packages("ggplot2") #install package used for creating plots
library(ggplot2) 
theme_set(theme_classic()) #set aesthetic theme for all plots


# Compute new variables from existing data--------------------------------------

### Compute new time-based variables

### Compute growing season cumulative precipitation


# Summarize data by group/s----------------------------------------------------

### Summarize precipitation and air temperature by year


### Output R dataframe to your computer


# Run statistical models--------------------------------------------------------

### test for a significant correlation between mean annual air temperature and total cumalative growing season precipitation


### test if the mean annual air temperature at KBS has increased over time


### test if above relationship remains after accounting for precipitation


### test affects of two independent variables and their interaction
data("ToothGrowth") #load built-in data on effects of vitamin C doses and supplement types ("supp") on guinea pig tooth length


### use a statistical test on a subset of the data


### logistic regression for binary dependent variable
credit_dat = read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/ISLR/Default.csv") #download dataset on customers defaulting on credit card debt


######################## Group Activity ##############################
#______________ KBS Resource Gradient Experiment_______________________

#Task: Read in data from the KBS Resource Gradient Experiment (https://lter.kbs.msu.edu/datatables/77.csv), Clean the data, summarize the data, and run a statistical test.

### Read data and change data structure

### Check for outliers

### Summarize data by crop, fertilizer rate, and irrigation

### Visualize summarized data

### Use a statistical model to test which variables affect yield

### Use a statistical model on just a subset of the data - a single crop type









