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
#
head(df_train)
summary(df_train)
#
glimpse(df_train)
#
str(df_train)


## number of female survived
female_count <- nrow(df_train %>% filter(Sex == 'female')) ## total number of female passenger
female_survived <- nrow( df_train %>% filter(Sex == 'female',Survived == 1) ) ## total number of female passengers survived.
print (sprintf("Total No. of Female Survived   = %2.2f %%", 100 * (female_survived/female_count))) # percentage of female survived

## number of male survived.
male_count <- nrow(df_train %>% filter(Sex == 'male')) ## total number of male passengers
male_survived <- nrow( df_train %>% filter(Sex == 'male',Survived == 1) ) ## total number of male survived.
print (sprintf("Total No. of Male Survived   = %2.2f %%", 100 * (male_survived/male_count))) # percentage of male survived.

## Total percentage of people survived.
print (sprintf("All-Survived   = %2.2f %%", 100 * (nrow( df_train %>% filter(Survived == 1) )/nrow(df_train))))
## Total percentage of people dead.
print (sprintf("All-Not Survived   = %2.2f %%", 100 * (nrow( df_train %>% filter(Survived == 0) )/nrow(df_train))))

## run test for check sex= male and predict survived.
head(df_test)
df_test$Survived <- ifelse(df_test$Sex == "male",1,0) ## if male survived =1 else female survived =0
head(df_test)
out <- df_test%>% select(PassengerId,Survived)
write.csv(out,"data-cleanup/r-sub-01.csv",row.names = FALSE)

## run test for check sex=female
df_test$Survived <- ifelse(df_test$Sex == "female",1,0)
head(df_test)
out <- df_test %>% select(PassengerId,Survived)
write.csv(out,"data-cleanup/r-sub-02.csv",row.names = FALSE)


########### Prediction based on Age ################
####################################################
table_1 <- table(df_train$Age,df_train$Survived)
table_1
#View(table_1)

