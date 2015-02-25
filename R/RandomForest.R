#################################################################################################
#################################################################################################
## Author: Bikash Agrawal
## Date: 24th Feb 2015
## Description Apply Random forest to predict the survival rate and create a new csv file.
## Kaggle titanic competition in kaggle.com
## Step 1:  Data clean up
## step 2:  Calculate probability of survival and dead.
## Step 3:  Calculate probability of survival based on gender (male, female).
## Step 4:  Calculate probabiliy of survival based on Age.
## Step 5:  Calcualtion of survival probability using decision tree.
#################################################################################################
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

########Cleaning up training dataset #####################################
head(df_train)

## get probability of people died and survived############################
survival_prop = prop.table(table(df_train$Survived)) 
## survival_prop[[1]] = 0.61 ( dead) and survival_prop[[2]]= 0.383 ( probability of alive or survived)
### 38% of passengers survived the titanic disaster from the given training set.
### Using this as probability model we can predict 61% of new test data will die.

### Data clean up add passenger ID and survival record to this file train_data.csv
out <- data.frame(PassengerID = df_train$PassengerId, Survived= df_train$Survived)
write.csv(out,"data-cleanup/train_data.csv",row.names = FALSE)
##########################################################################

df_train_1 <- df_train %>% select(-Name,-Ticket,-Cabin,-Embarked,-Age)
head(df_train_1)

### Survival probability based on gender #################################
##########################################################################
survival_prop_sex = prop.table(table(df_train$Sex,df_train$Survived),1) 
## 1 stand for using rowwise probability and 2 will give column proportions.
################ output #################################################
###                     0          1
###       female    0.2579618   0.7420382  ## majority of female survived
###       male      0.8110919   0.1889081
##########################################################################
################## Using prediction based on sex##########################
df_test$Survived <- 0
df_test$Survived[df_test$Sex == 'female'] <- 1
### This model is based on assumption of past data that majority of female will survive.

#### Survival probability based on Age ###################################
##########################################################################
summary(df_train$Age)



##########################################################################
##### Prediction using Decision Tree ####################################
##########################################################################
library(rpart)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=df_train, method="class")
plot(fit)
text(fit)

### Make a fancy plot for decision tree #################################
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(fit)
# prediction
Prediction <- predict(fit, df_test, type = "class")
out <- data.frame(PassengerID = df_test$PassengerId, Survived = Prediction)
write.csv(out, file = "Decision_tree.csv", row.names = FALSE)

######## cleaning up test dataset           ##############################
##########################################################################
df_test_1 <- df_test %>% select(-Name,-Ticket,-Cabin,-Embarked,-Age)
df_test_1$Survived <- NULL # Delete the column if we have survived
str(df_test_1)
#
##########################table(train$Survived)##########################
### Random Forest #######################################################
#########################################################################
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


