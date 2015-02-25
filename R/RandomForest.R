## Author Bikash Agrawal
## Description This file clean up the original datasets and extract passengerId and survial information
## and create a new csv file.
## Date: 24th Feb 2015
## Kaggle titanic competition

getwd()
setwd("/Users/bikash/repos/RFKaggleTitanic/")
#
print("Data Cleaning up process......")
rm(list = ls(all = TRUE))

library(dplyr)
sessionInfo()
df_train = read.csv('titanic-data/train.csv')
df_test = read.csv('titanic-data/test.csv')
head(df_train)
summary(df_train)
glimpse(df_train)
str(df_train)