#### Setup your work space (5 min) - 5 #### 

### clear your environment
rm(list = ls())
### Restart R session to remove loaded packages
.rs.restartR()

### load packages
library(tidyverse)

### Set working directories with `getwd()` and `setwd()` 
getwd()
setwd("C:/R_Projects/teaching_R")



#### Loading data (10 min) - 15 #### 
# Learning goals: assignment, excel to csv, naming columns, loading data, data summary (`str()`, `View()`, `names()`, `summary()`)

# Reads in comma separated values into R
# The file is named 'KBS_R_workshop_session2_hesperis_data.csv'
read_csv("KBS_R_workshop/KBS_R_workshop_session2_hesperis_data.csv")

# We need to assign what is read into an object to keep working with the object
d <- read_csv("KBS_R_workshop/KBS_R_workshop_session2_hesperis_data.csv")


# color = Flower color of Dame's rocket ("purple" or "white")
# site = Study site numbered 1 to 5
# height = Maximum height of the plant in cm
# number of leaves = Number of leaves
# flower_temp = temperature of the flower petals in C

# Print our data.frame
d

# Display the dimension, column names, column class
str(d)

# View the entire data.frame
View(d)

# Get only the column names
names(d)


# Get a summary of the data.frame
summary(d)



#### R syntax / Boolean review (15 min) - 30 #### 
# Learning goals: Logic table, equality, inequality, complement

# Index site
d$site

# Check if each value of 'site' equals 2
d$site == 2

# Check if each value of 'site' does not equal 2
d$site != 2 
!(d$site == 2)

# Check if each value of 'site' is greater than 2
d$site > 2

# Logic table
T & T # T
T & F # F
F & F # F

F | T # T
F | F # F
T | F # T

# find where "site" equals 2 *or* "site" equals 3
d$site == 2 | d$site == 3

# find where "height" is greater than 100 *and* "color" is purple
d$height > 100 & d$color == "purple"


### Challenge 1 ###
# find where "site" is not 2 and "color" is "white"
d$site != 2 & d$color == "white"



#### Data subsetting (15 min) - 45 #### 
# Learning goals: Base R subsetting with `[]`, `$`, `select()`, `filter()`

# index column with `$`
names(d)
d$height


# index column with `select()`. 
# Provide the column names with `,` separating each of them 
select(.data = d, height, color)

# With tidyverse, the first argument is usually the `.data` argument, so we can use piping 
d %>% 
  select(height, color)


# Let's try to index "number of leaves"

# Why does the code return an error?

# Let's fix the naming issue 
# First let's index the problematic column name using `[]`
# Provide the thing we want to index from on the left of `[]` and provide the position of the element you want to index inside `[]`
names(d)
names(d)[4]

# Let's reassign the value with the proper naming convention 'num_leaves'
names(d)[4] <- "num_leaves"

# Check our work. Is this what we expect?
names(d)

# index rows
# Provide Boolean conditions in `filter()`
d$color == "purple"

d %>% 
  filter(color == "purple")

# index by multiple boolean conditions
# Both site == 3 and color = "purple"

d %>% 
  filter(site == 3 & color == "purple")

### Challenge 2 ###
# Let's check out our data again. Notice that "flower_temp" has a maximum of 24690! Something is wrong.
summary(d)

# Try to fix the entry error in the data set and define it as a new data set called "d_cleaned"
d_cleaned <- d %>% 
  filter(flower_temp < 100)

d_cleaned



#### Performing calculations on columns (20 min) - 1:05 #### 
# Learning goals: `group_by()`, `summarise()` or `summarize()`, `mutate()`, `ungroup()`.

# Lets try to find the average height of different color morphs
# Maybe start with `mean()`?
mean(d_cleaned$height) # This gives us the global height

# We can try to filter the data.frame by flower color then select the height to find the average
d_cleaned %>% 
  filter(color == "purple") %>% 
  select(height) %>% 
  unlist() %>% 
  mean()

d_cleaned %>% 
  filter(color == "white") %>% 
  select(height) %>% 
  unlist() %>% 
  mean()

# Very tedious! How to we do this faster?

# Calculate the summary statistics for different variables
d_cleaned %>% 
  group_by(color) %>% # Find summary statistics for each unique value of "color"
  summarise(mean_height = mean(height), # Provide new column name = function(value)
            max_temp = max(flower_temp), 
            sd_num_leaves = sd(num_leaves)) %>% 
  ungroup() # Finish by running the `ungroup()` function to avoid bugs

# Grouping by multiple variables
d_cleaned %>% 
  group_by(color, site) %>% 
  summarise(mean_height = mean(height), 
            max_temp = max(flower_temp), 
            sd_num_leaves = sd(num_leaves)) %>% 
  ungroup()


### Challenge 3 ###
# Try to find the lowest recorded flower temperature at each site
d_cleaned %>% 
  group_by(site) %>% 
  summarise(min_temp = min(flower_temp))


# Create a new column with `mutate()`
# Let's create a new column called 'error' and assign it some value
d %>% 
  mutate(error = "uhhh, I don't know")

# Run `d` again
d
str(d)

# Do you see the "error" column? Why not? 


# We can provide the scoring criteria base on the value of another column
d %>% 
  mutate(error = flower_temp > 100)

# Let's remember to save our work
d <- d %>% 
  mutate(error = flower_temp > 100)

# Let's overwrite our flower_temp column by replacing the entry error with 'NA'
# We can use `ifelse(boolean condition, value if TRUE, value if FALSE)`
d <- d %>% 
  mutate(
    flower_temp = ifelse(error, NA, flower_temp)
  )

# What if we want the maximum temperature of the plant at each site?
# We can calculate the group summary statistics and add it as a column without reshaping the data.frame
d_cleaned %>% 
  group_by(site) %>% 
  mutate(max_temp = max(flower_temp)) %>% 
  ungroup() %>% 
  as.data.frame() # tibble hides the data.frame when it is too long.


### Challenge 4 ###
# You want to create a plant size index using the plant height and number of leaves to find the larger plants to collect seeds from. 
# The plant size index has the formula `size_index = height/100 + num_leaves^0.5`
# Can you create a new column called 'size_index' and then subset by plants with a size_index greater than 8?
d_cleaned %>% 
  mutate(size_index = height / 100 + num_leaves^0.5) %>% 
  filter(size_index > 8)





#### data.frame transformation (10 min) - 1:15 #### 
# Learning goals: left_join()

# Create a data.frame for site
site_data <- data.frame("site" = c(1:5), 
                        "soil_moisture" = c(5, 10, 15, 33, 0.4), 
                        "shadiness" = c("shady", "sunny", "shady", "sunny", "sunny"))

# Check out our data.frame object
str(site_data)

# How do we bind the site level data to our plant level data? 
d_cleaned %>% 
  left_join(site_data, by = "site")

# Remember to save our changes in something new or redefine our original object
d_merged <- d_cleaned %>% 
  left_join(site_data, by = "site")
d_merged






#### Formula syntax (15 min) - 1:30 #### 
# Learning goals: `lm()`, `~`, `summary()`, `plot()`

# The syntax for a formula object in R:
# Response_variable ~ predictor_variables + predictor_variables 

# Say we want to see if taller plants had warmer flowers
# Provide the data argument with data = your_data_frame_name

# Let's look at the plot of "flower_temp" vs "height" using `plot()`. 
plot(flower_temp ~ height, data = d_cleaned)

# Same syntax using the formula of our linear model
# Let's fit the model using the `lm()` function.
lm(flower_temp ~ height, data = d_cleaned)

# Let's save our model output and look at more details with `summary()`
m <- lm(flower_temp ~ height, data = d_cleaned)

summary(m)

# What does the "Estimate" mean?
# What does the "Pr(>|t|)" mean?

# Let's return to the plot
# Let's draw the line of our fitted linear model using `abline()` after excuting `plot()`
plot(flower_temp ~ height, data = d_cleaned)
abline(m)


######################################################################
######################## Group Activity ##############################
######################################################################
# Read in the "KBS_R_workshop_session2_food_data.csv" file and assign it to the object 'food_data'
food_data <- read.csv("KBS_R_workshop/KBS_R_workshop_session2_food_data.csv")

str(food_data)

# name = name of the recipe
# type = a "cookie", "cake", "muffin", "bread", or "bagel"
unique(food_data$type) # Try to run this to see all unique values
# sugar = volume of sugar in mL
# oil = volume of oil in mL
# flour = volume of flour in mL
# egg = number of eggs
# leavening = volume of baking soda or baking powder in mL
# liquid = volume of milk, butter milk, water, lime juice, etc in mL
# yeast = volume of yeast in mL


# Can you find the volume of eggs needed and defined it as a new column called "egg_vol"? 
# A large egg is around 46 mL
food_data <- food_data %>% 
  mutate(egg_vol = 46 * egg)

# Can you find the approximate total volume of the batter by adding up all the ingredients?
food_data <- food_data %>% 
  mutate(tot_vol = egg_vol + sugar + oil + flour + leavening + liquid + yeast)

# Now, try to calculate the proportion of each ingredient out of the total volume
food_data <- food_data %>% 
  mutate(egg_vol = egg_vol / tot_vol,
         sugar = sugar / tot_vol,
         oil = oil / tot_vol,
         flour = flour / tot_vol,
         leavening = leavening / tot_vol,
         liquid = liquid / tot_vol,
         yeast = yeast / tot_vol)

# Read in the rating score data for each of the recipe ("KBS_R_workshop_session2_rating_data.csv")
ratings_data <- read.csv("KBS_R_workshop/KBS_R_workshop_session2_rating_data.csv")
str(ratings_data)

# Combine the ratings data with the food data
names(ratings_data)

food_data <- food_data %>% 
  left_join(ratings_data, by = "name")

# Find the average proportion of ingredients for each type of baked goods
food_data %>% 
  group_by(type) %>% 
  summarise(
    sugar = mean(sugar), 
    oil = mean(oil), 
    flour = mean(flour), 
    egg_vol = mean(egg_vol), 
    leavening = mean(leavening), 
    yeast = mean(yeast), 
    liquid = mean(liquid)
  )


# find all the "cookie" recipes and choose only the "leavening" and "rating" columns
# Put this in a new data.frame called 'cookie_data'
cookie_data <- food_data %>% 
  filter(type == "cookie") %>% 
  select(leavening , rating)


# Fit a linear model and make a plot of rating vs leavening for this 'cookie_data'
# Do recipes that add more leavening get rated higher? 
cookie_m <- lm(rating ~ leavening, data = cookie_data)
summary(cookie_m)

plot(rating ~ leavening, data = cookie_data)
abline(cookie_m)



# It's kind of hard to work with a very wet batter. Maybe there is a negative correlation between the amount of liquid and the amount of oil you add to the batter. 
# Using the whole food_data dataset, try to test if this hypothesis is true
m_wet_batter <- lm(liquid ~ oil, data = food_data)
summary(m_wet_batter)

plot(liquid ~ oil, data = food_data)
abline(m_wet_batter)


# Explore the data set! Answer some of life's greatest questions. 
# Suggestions: 
## Are yeastier bagels tastier? 
## Which type of baked goods has the highest average rating?
## If we have a lot of leavening already, do we need more yeast?
## Are bagels just a type of bread?
## Are muffins just wet cakes?
## What is the defining feature of bread and bagels?
## What would you make if you add milk to a cookie batter?


