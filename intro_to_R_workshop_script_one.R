### Welcome to the Introduction to R for Ecologists Workshop

### clear your environment
rm(list = ls())
### unload user installed packages 
invisible(lapply(paste0("package:", names(sessionInfo()$otherPkgs)), detach,character.only = TRUE, unload = TRUE))

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

number_list = c(21, 34, 39, 54, 55)
class(number_list)
str(number_list)
length(number_list)
number_list*5
number_list[number_list < 30 | number_list > 50]

word_list = c("Dog","Cat","Zebra","Jaguar","Salmon")
str(word_list)
word_list[1]
word_list[length(word_list)]
word_list[4:2]
word_list[word_list!="Zebra"]

heights <- c(2, 4, 4, NA, 6)
max(heights)
max(heights,na.rm=TRUE)
heights[!is.na(heights)]
4 %in% heights
"NA" %in% heights
NA %in% heights

?factor()
trees=factor(c("oak",'hickory','maple','hickory',"maple",'oak'))
?levels()
levels(trees)
?unique()
unique(trees)

animal=c(rep("lion",times=10000),rep('gazelle',times=10000))
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

?sample()
die <- 1:6
### create a function that rolls 2 die and sums their results
roll = function(die) { 
  dice <- sample(die, size = 2, replace = TRUE)
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

sky='blue'
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
###################### Blackjack Simulator ###########################
######################################################################

# create a dataframe that represents a deck of cards

deck=data.frame(index=1:52,
                rank=rep(c(2:10,10,10,10,11),4),
                name=rep(c(2:10,'Jack','Queen','King','Ace'),4),
                suit=c(rep('Spade',13),
                       rep('Club',13),
                       rep('Heart',13),
                       rep('Diamond',13)))
View(deck)

# shuffle the cards into a random order

shuffle = function(x) {x[sample(1:nrow(x)), ]}
head(deck)
deck=shuffle(deck)
head(deck)
# create a function that deals 2 cards 
# then deal two cards to the dealer and two to the player

deal = function(x) {x[sample(1:nrow(x),2,replace=F), ]}

dealer=deal(deck)
player=deal(deck)

head(dealer)
head(player)

# write a function that checks the current sum of a player's hand
# then create a 'hit' function that adds one card to a player's hand
# then create an if statement that decides if the dealer should hit 
# and if so, add one card to the dealer's hand

current_sum=function(x) {sum(x$rank)}
current_sum(dealer)

hit = function(x) {x[sample(1:nrow(x),1,replace=F), ]}

for (i in 1:10) {
if (current_sum(dealer)<17) {
  dealer=rbind(dealer,hit(deck))
}
}

head(dealer)
current_sum(dealer)

# write a function that decides if the dealer's hand has gone over 21

bust=function(x) {
  if(current_sum(x)>21){
    print('Bust!')
  } else {
    print('Not bust')
  }
}

bust(dealer)

# use the hit function to add a card to the player's hand at your desecration

head(player)
current_sum(player)
stay= TRUE
if (stay==TRUE) {
  player=rbind(player,hit(deck))
}
head(player)
current_sum(player)

# right a function to decide who won the round, then check who won

winner=function(dealer,player) {
  if ((dealer > 22) & (player < 22)) {
    print('Player wins!')
  }
  if ((player > 22) & (dealer < 22)) {
    print('Player wins!')
  }
  if ((player > dealer) & (player < 22)) {
    print('Player wins!')
  }
  if ((dealer > player) & (dealer < 22)) {
    print('Dealer wins!')
  }
  if ((dealer == player) & (dealer < 22)) {
    print('Tie')
  }
}

current_sum(dealer)
current_sum(player)
winner(current_sum(dealer),current_sum(player))

# BONUS, write a script that allows one player to play blackjack against 
# the dealer, and loop over this 1,000 times to see who wins in the long run.




