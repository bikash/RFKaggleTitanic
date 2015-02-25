## Author Bikash Agrawal
## Description Apply Random forest to 
## and create a new csv file.
## Date: 24th Feb 2015
## Kaggle titanic competition in kaggle.com

#################################################################################################

### setting path of repo folder.
getwd()
setwd("/Users/bikash/repos/RFKaggleTitanic/")

print("Data Cleaning up process......")

library(dplyr)
sessionInfo()
df_train = read.csv('titanic-data/train.csv') ## read data from csv file
df_test = read.csv('titanic-data/test.csv')
head(df_train)
summary(df_train)
glimpse(df_train)
str(df_train)

########Cleaning up training dataset ######################################
head(df_train)

## get probability of people died and survived############################
survival_prop = prop.table(table(df_train$Survived)) ## survival_prop[[1]] = 0.61 ( dead) and survival_prop[[2]]= 0.383 ( probability of alive or survived)
### 38% of passengers survived the titanic disaster from the given training set.

### Data clean up add passenger ID and survival record to this file train_data.csv
out <- data.frame(PassengerID = df_train$PassengerId, Survived= df_train$Survived)
write.csv(out,"data-cleanup/train_data.csv",row.names = FALSE)

df_train_1 <- df_train %>% select(-Name,-Ticket,-Cabin,-Embarked,-Age)
head(df_train_1)

######## cleaning up test dataset           ##############################
##########################################################################
df_test_1 <- df_test %>% select(-Name,-Ticket,-Cabin,-Embarked,-Age)
df_test_1$Survived <- NULL # Delete the column if we have survived
str(df_test_1)
#
##########################table(train$Survived)#####################################
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


