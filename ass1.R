# Load required libraries
library(dplyr)
library(ggplot2)


# Load data

data <- read.table(unz("activity.zip", "activity.csv"), header=T, quote="\"", sep=",")

# 1. Calc total steps per day
sum(data$steps,na.rm=TRUE)

# 2. Plot histogram of daily steps
hist(data$steps)

# 3. Calculate and report the mean and median of the total number of steps taken per day
mean(data$steps,na.rm=TRUE)
median(data$steps,na.rm=TRUE)
sum(data$steps,na.rm=TRUE)
