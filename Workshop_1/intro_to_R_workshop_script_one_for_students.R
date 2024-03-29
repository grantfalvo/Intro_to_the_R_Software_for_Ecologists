### Welcome to the Introduction to R for Ecologists Workshop

### clear your environment
rm(list = ls())
### unload user installed packages 
invisible(lapply(paste0("package:", names(sessionInfo()$otherPkgs)), detach,character.only = TRUE, unload = TRUE))

### Using R as a calculator
4+4
x=5
y=6.447
z=x+y
args(round)
round(z,2)
round(digits=2,x=z)
?round()
z %>% round(2)
library(dplyr)
z %>% round(2)

### Learning about vectors of numbers
number_list = c(21, 34, 39, 54, 55)
class(number_list)
str(number_list)
length(number_list)
number_list*5
number_list[number_list < 30 | number_list > 50]
number_list < 30

### Learning about vectors of strings
word_list = c("Dog","Cat","Zebra","Jaguar","Salmon")
str(word_list)
word_list[5]
word_list[length(word_list)]
word_list[4:2]
word_list[word_list!="Zebra"]

### Querying vectors of numbers
heights <- c(2, 4, 4, NA, 6)
max(heights)
max(heights,na.rm=TRUE)
heights[!is.na(heights)]
4 %in% heights
"NA" %in% heights
NA %in% heights

### Learning about factors
?factor()
trees=factor(c("oak",'hickory','maple','hickory',"maple",'oak'))
?levels()
levels(trees)
?unique()
unique(trees)

### Creating and exploring a dataset 
animal=c(rep("lion",times=10000),
         rep('gazelle',times=10000))
speed=c(rnorm(n=10000,mean=50,sd=10),rnorm(n=10000,mean=60,sd=10))
dat=data.frame(animal=animal,speed=speed)
dat
head(dat)
str(dat)
dat$animal
dat$animal=as.factor(dat$animal)
boxplot(dat$speed~dat$animal)

dat$animal %in% "lion"
### dataframe[rows,columns]
dat[dat$animal %in% "lion",2]
dat[dat$animal %in% "lion",2] %>%
  hist(breaks=50)

### Pop quiz
### What does this line of code do? 
ugz=dat[(animal=='gazelle' & speed < mean(subset(dat,animal=='lion')$speed)),]

boxplot(subset(dat,animal=='lion')$speed,subset(dat,animal=='gazelle')$speed,ugz$speed,
        names=c('lion','gazelle','unlucky gazelles'))
abline(h=mean(subset(dat,animal=='lion')$speed))

### Custom Functions, If else statements, and For Loops

my_function = function() {} ### generic function form

### Create a function that rolls 2 die and sums their results
?sample()
die = 1:6
roll = function(die) { 
  dice = sample(die, size = 2, replace = TRUE)
  sum(dice)
}

roll(die)
?replicate()
rolls <- replicate(10000, roll(die))
?hist()
hist(rolls)

### If/else statements
animal=c(rep("lion",times=10000),rep('gazelle',times=10000))
speed=c(rnorm(n=10000,mean=50,sd=10),rnorm(n=10000,mean=60,sd=10))
dat=data.frame(animal=animal,speed=speed)

# if (logical condition is true) {
#   then do x
#  } else {
#      do y instead
#  }

sky='red'
  if (sky=='blue'){
    print("the sky is blue")
  } else { 
    print("the sky is not blue")
  }

if (subset(dat,animal=='gazelle')[sample(length(dat[,2])/2,1),2] 
    < mean(subset(dat,animal=='lion')$speed)) {
  print("slow gazelle")
} else {
  print("fast gazelle")
}

### For loops
for (year in 2010:2015){
  print(paste("The year is", year))
}

flip = function() {sample(c("T", "H"), 1)}
flip()

flips = 0
heads = 0
tails = 0
for (i in 1:10000) {
  if (flip() == "H") {
    heads = heads + 1
  } else {
    tails = tails + 1
  }
  flips = flips + 1
}
heads/flips
tails/flips

###

######################################################################
######################## Group Activity ##############################
######################################################################

######################################################################
###################### Plant Growth Simulator ########################
######################################################################

### create a function that outputs the growth in height of a plant
### in response to temperature and light



### create a dataframe that gives the monthly temperature and sun light
### for a location over the course of a year



### use your function and dataframe to model the growth in height of 
### a plant at your location over the course of a year



### plot your plant's growth over time



### Bonus activity:
### Modify your plant growth function to give different growth rates to
### different types of plants. Then model the response of two or more plant
### types to the same weather conditions.

