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

########Simple tree model######################################
head(df_train)
df_train_1 <- df_train %>% select(-Name,-Ticket,-Cabin,-Embarked,-Age)
head(df_train_1)

######## transform the test data##############################
#############################################################
df_test_1 <- df_test %>% select(-Name,-Ticket,-Cabin,-Embarked,-Age)
df_test_1$Survived <- NULL # Delete the column if we have survived
str(df_test_1)
#
###############################################################
### Random Forest #############################################
###############################################################
library(randomForest)
mdl_rf <- randomForest(Survived ~ Pclass + Sex + SibSp+Parch+Fare, data = df_train_1,importance = TRUE)
mdl_rf
importance(mdl_rf)
summary(df_test_1)
df_test_1$Fare[is.na(df_test_1$Fare)] <- 35.0

## prediction
pred <- predict(mdl_rf,newdata = (df_test_1 %>% select(-PassengerId)))
pred
df_test_1$Survived <- pred
head(df_test_1)
out <- df_test_1 %>% select(PassengerId,Survived)
write.csv(out,"data-cleanup/r-prediction-rf.csv",row.names = FALSE)
#