########### Machine Learning #############
## Pre-Assessment #######
library(tidyverse)
library(dslabs)
data("heights")
head(heights)
class(heights$height)
class(heights$sex)
str(heights)
x = heights[777,]
x
heights$sex[777]
x = min(heights$height)
z = mean(heights$height)
summary(heights)
sum(heights$height > 78)        
summary(heights$sex)
812/(812+238)
f = filter(heights, sex == 'Female')
sum(f$height > 78)
which.min(heights$height)

######################################################################################################################
### Notation #####
# Y denotes the outcomes, X denotes the feature (predictors or covariates)
# generally, we have series of features and an unknown outcome we would like to predict

###### The basics #########

# Start to use the caret package.
# Construct and interpret a confusion matrix.
# Use conditional probabilities in the context of machine learning.
# the general approach to build the algorithm is to develop from the known dataset. 
# The dataset then split into two as test and train dataset
# we will try to mimic and construct the algorithm in the train set
# a standard way of generating the training and test sets is by randomly splitting the data.
# the caret package can perform createDataPartition function that helps generating indexes for randomly splitting the data into training and tests sets

library(tidyverse)
library(caret)
library(dslabs)
data(heights)

# the first example is trying to predict sex based on height
# define the outcome and predictors

y = heights$sex
x = heights$height

# generate training and test sets

set.seed(2, sample.kind = 'Rounding')
test_index = createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set = heights[test_index,]
train_set = heights[-test_index,] # split dataset into two groups


##### Overall accuracy ##########
# guess the outcome
y_hat = sample(c('Male', 'Female'), length(test_index), replace = TRUE)

# Note that we are completely ignoring the predictor and simply guessing the sex.

# In machine learning applications, it is useful to use factors to represent the categorical outcomes because R functions developed for machine learning, such as those in the caret package, require or recommend that categorical outcomes be coded as factors. So convert y_hat to factors using the factor function:

y_hat = sample(c('Male', 'Female'), length(test_index), replace = TRUE) %>% 
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex) # due to our naive guessing, the overall accuracy is only 50%

# to do it better, based on knowledge in the past, Males are slightly taller than females in average
heights %>% group_by(sex) %>% summarise(mean(height), sd(height))

# But how do we make use of this insight? Let’s try another simple approach: predict Male if height is within two standard deviations from the average male:

y_hat = ifelse(x > 64, 'Male', 'Female') %>% 
  factor(levels = levels(test_set$sex))

mean(y == y_hat) # overall accuracy is now 80%

# we arbitratily picked 62 inches, which mean if we pick a better cutoff point the prediction should be improved 

cutoff = seq(61,70)
accuracy = map_dbl(cutoff, function(x) {
  y_hat = ifelse(train_set$height > x, 'Male', 'Female') %>% 
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})


#checking the data by plotting
data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 

max(accuracy) # around 83%
# thus, the best cutoff point is
best_cutoff = cutoff[which.max(accuracy)]
best_cutoff

## apply the cutoff to the test set
accuracy = map_dbl(best_cutoff, function(x) {
  y_hat = ifelse(test_set$height > x, 'Male', 'Female') %>% 
    factor(levels = levels(test_set$sex))
  mean(y_hat == test_set$sex)
})
accuracy  # 80% correct

##### The confusion matrix #######
# the prediction rule we developed would predict 'Male' if the student is taller than 64 inches
# Given that the average female is about 64, this prediction seems wrong
# overall accuracy can be deceptive. confusuion matrix can solve this

# # tabulate each combination of prediction and actual value
# table(predicted = y_hat, actual = test_set$sex)
# test_set %>% 
#   mutate(y_hat = y_hat) %>%
#   group_by(sex) %>% 
#   summarize(accuracy = mean(y_hat == sex))
# prev <- mean(y == "Male")
# 
# confusionMatrix(data = y_hat, reference = test_set$sex)

# ########### Balanced accuracy and F1 score #############
# test_set %>% 
#   mutate(y_hat = y_hat) %>%
#   group_by(sex) %>% 
#   summarize(accuracy = mean(y_hat == sex))

cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})
F_1
############# ROC and precision-recall curves ###################
# When comparing between methods, ROC is a widely used method to compare the performance between methods
# ROC curve plots sensitivity (TPR) vs 1-specificity or the false positive rate (FPR)

library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

# Get the basic data as shown
tns <- length(y)                 # total number in class
pic <- mean(x == "inclass")      # percentage in class
pol <- mean(x == "online")       # percentage online
nic <- tns * pic                 # number in class
nol <- tns * pol                 # number online

# Create a vector fol which lists all females in dat who are in class
# Then create the ratio of that number (length(fol)) and the total in class (nic)
fol <- which(dat$sex == "Female" & dat$type == "inclass")
pfic <- length(fol) / nic
pfic

# Do the same for femails who are online
fic <- which(dat$sex == "Female" & dat$type == "online")
pfol <- length(fic) / nol
pfol

set.seed(1)
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))



library(tidyverse)
library(HistData)

galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)


library(caret)
y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)

avg <- mean(train_set$son)
avg

mean((avg - test_set$son)^2)

# fit linear regression model
fit <- lm(son ~ father, data = train_set)
fit$coef

y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
mean((y_hat - test_set$son)^2)

# Comprehension Check: Linear Regression
library(tidyverse)
library(caret)
library(dslabs)
library(dplyr)
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

# Replication
set.seed(1)
results <- replicate(100, expr = {
  
  # Partition the dataset into test and training sets of equal size
  train_index <- createDataPartition(dat$y, p = .5, list = FALSE, times = 1)
  train_set <- dat[-train_index,]
  test_set <- dat[train_index,] 
  
  # Train the model
  model <- lm(y ~ x, data = train_set)
  model_prediction <- predict(model, test_set) 
  
  # Calculate the RMSE
  model_result <- test_set$y - model_prediction
  model_rmse <- sqrt(mean(model_result^2))
})

# question 2



########### Smoothing #############
# the general idea of smoothing is to group data points into strata in which the value of f(x) can be assummed constant
# we can make this assumption becuase we think f(x) changes slowly and, as a results, f(x) is almost constant. 
#The example can be seen on the poll_2008 data. This data assume that public opinion remained approxiamtely the same within a week's time.
# with this assumption in place, we have several data points with the same expected value
# fix the day to be the centre of the week as x0 then for any other day x such that |x-x0| <= 3.5, we assume f(x) is a constant f(x) = μ



