#################################################################################################
#################################################################################################
## Author: Bikash Agrawal
## Date: 24th Feb 2015
## Email: er.bikash21@gmail.com
## Description: Apply Random forest to predict the survival rate and create a new csv file. This algorithm might helps to be in top 10 in Kaggle competition.
##              Kaggle titanic competition in kaggle.com.
##              http://www.kaggle.com/c/titanic-gettingStarted
## Step 1:  Data clean up
## step 2:  Calculate probability of survival and dead.
## Step 3:  Calculate probability of survival based on gender (male, female).
## Step 4:  Calculate probabiliy of survival based on Age.
## Step 5:  Calcualtion of survival probability using decision tree.
## Step 6:  Calculate probability of survival using Random Forest.
## Step 7:  Calculate probability of survival using Condition Inference Random Forest.
## Step 8:  Calculate probability of survival using Support Vector Machine.
#################################################################################################
#################################################################################################

### setting path of repo folder.
getwd()
setwd("/Users/bikash/repos/RFKaggleTitanic/")

library(dplyr)
sessionInfo()


##########################################################################
##### Function to calculate prediction error #############################
##########################################################################
predict_error<-function(test,test1)
{
    error = test-test1
    count = length(test)
    error.per = sum(error)/count
}
##########################################################################
##########################################################################

##########################################################################
########Cleaning up training dataset #####################################
##########################################################################
print("Data Cleaning up process......")
df_train = read.csv('titanic-data/train.csv') ## read data from csv file
df_test = read.csv('titanic-data/test.csv')
head(df_train)
summary(df_train)
glimpse(df_train)
str(df_train)
head(df_train)

###########Get missing data information###################################
library(Amelia)
missmap(df_train, main = "Missing Map train")
missmap(df_test, main = "Missing Map Test") 
## we can see age is badly missing in both the datasets

## get probability of people died and survived############################
survival_prop = prop.table(table(df_train$Survived)) 
## survival_prop[[1]] = 0.61 ( dead) and survival_prop[[2]]= 0.383 ( probability of alive or survived)
### 38% of passengers survived the titanic disaster from the given training set.
### Using this as probability model we can predict 61% of new test data will die.

### Data clean up add passenger ID and survival record to this file train_data.csv
out <- data.frame(PassengerId = df_train$PassengerId, Survived= df_train$Survived)
write.csv(out,"data-cleanup/train_data.csv",row.names = FALSE)
##########################################################################

###step 2:
##########################################################################
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
out <- data.frame(PassengerId = df_test$PassengerId, Survived = df_test$Survived)
write.csv(out, file = "data-cleanup/gendermodel-predict.csv", row.names = FALSE)
### This model is based on assumption of past data that majority of female will survive.
##########################################################################
##########################################################################

###Step 3:
##########################################################################
#### Survival probability based on Age ###################################
##########################################################################
summary(df_train$Age)
df_train$Child <- 0
df_train$Child[train$Age < 18] <- 1
aggregate(Survived ~ Child + Sex, data=df_train, FUN=sum)
aggregate(Survived ~ Child + Sex, data=df_train, FUN=length)
aggregate(Survived ~ Child + Sex, data=df_train, FUN=function(x) {sum(x)/length(x)})
# Look at class and fare patterns
df_train$Fare2 <- '30+'
df_train$Fare2[df_train$Fare < 30 & df_train$Fare >= 20] <- '20-30'
df_train$Fare2[df_train$Fare < 20 & df_train$Fare >= 10] <- '10-20'
df_train$Fare2[df_train$Fare < 10] <- '<10'
aggregate(Survived ~ Fare2 + Pclass + Sex, data=df_train, FUN=function(x) {sum(x)/length(x)})
# runing prediciton 
df_test$Survived <- 0
# Update the prediction to say that all females will survive
df_test$Survived[df_test$Sex == 'female'] <- 1
# Update once more to say that females who pay more for a third class fare
df_test$Survived[df_test$Sex == 'female' & df_test$Pclass == 3 & df_test$Fare >= 20] <- 0
out <- data.frame(PassengerId = df_test$PassengerId, Survived = df_test$Survived)
write.csv(out, file = "data-cleanup/agemodel-predict.csv", row.names = FALSE)
##########################################################################
##########################################################################

### Step 4:
##########################################################################
##### Prediction using Decision Tree  ####################################
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
write.csv(out, file = "data-cleanup/Decision_tree.csv", row.names = FALSE)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=df_train,
             method="class", control=rpart.control(minsplit=2, cp=0))
fancyRpartPlot(fit)
##########################################################################
##########################################################################

#step 5:
##########################################################################
##### Prediction using Random Forest   ###################################
##########################################################################
#Join together the test and train datasets
df_test$Survived <- NA
combi <- rbind(df_train, df_test)

# Convert to a string
combi$Name <- as.character(combi$Name)
# Engineered variable: Title
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
# Combine small title groups
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
# Convert to a factor
combi$Title <- factor(combi$Title)

# Engineered variable: Family size
combi$FamilySize <- combi$SibSp + combi$Parch + 1
# Engineered variable: Family
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
# Delete erroneous family IDs
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
# Convert to a factor
combi$FamilyID <- factor(combi$FamilyID)

# Fill in all the missing Ages NA's
summary(combi$Age)
###########################################################################################
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   0.17   21.00   28.00   29.88   39.00   80.00     263 
###########################################################################################
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])



summary(combi) ## check if something is still missing e.g Fare 
#Embarked, All missing Embarked -> just make them embark from most common place
summary(combi$Embarked) ## check for Embarked missing data
which(combi$Embarked == '')## get id for missing data ## 62 830
combi$Embarked[c(62,830)] = "S" ## fill it with S
combi$Embarked <- factor(combi$Embarked)

# All the missing Fares -> assume median of their respective class
summary(combi$Fare) ## check if fare is missing NA's ->1
which(is.na(combi$Fare)) ## get id for missing data
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE) ## add median fare to missing data

# New factor for Random Forests, only allowed <32 levels, so reduce number
combi$FamilyID2 <- combi$FamilyID
# Convert back to string
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
# And convert back to factor
combi$FamilyID2 <- factor(combi$FamilyID2)


# Split back into test and train data sets also it is necessary to remove all the missing data from 
## the data pool in order to apply Random Forest.
train <- combi[1:500,]
#test <- combi[892:1309,]
test <- combi[501:891,]
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked  + FamilySize + FamilyID2,
                    data=train, importance=TRUE, ntree=1000)
# Look at variable importance
importance(fit)
varImpPlot(fit)
#
fit$confusion
##########################################################################
##        0   1 class.error
##    0 277  30  0.09771987
##    1  51 142  0.26424870
##########################################################################

# Now let's make a prediction of survival rate and display in csv file. 
Prediction <- predict(fit, test)
out <- data.frame(PassengerID = test$PassengerId, Survived = as.numeric(Prediction))
#########################################################################
## calculate accuracy of model
accuracy = sum(Prediction==test$Survived)/length(Prediction)
print (sprintf("Accuracy = %3.2f %%",accuracy*100)) ### 81.84% accuracy of model using random forest
#########################################################################

### Mean square prediction error
out$Survived <- ifelse(out$Survived == 2,1,0)
actual.out <- data.frame(PassengerID = test$PassengerId, Survived = test$Survived)
MSPE = mean((out$Survived-actual.out$Survived)^2)
cat(" Mean Square prediction error is : -> ", MSPE)
##count = length(out$Survived)
#error.per = sum(error)/count ## 18 % error
#########################################################################
write.csv(out, file = "data-cleanup/randomForest-prediction.csv", row.names = FALSE)
##########################################################################
##########################################################################


##########################################################################
##### Prediction using Condition Inference Random Forest  ################
##########################################################################
library(cforest)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
#prediction
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
out <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(out, file = "data-cleanup/ciRandomForest-predict.csv", row.names = FALSE)


#########################################################################


##########################################################################
##### Prediction using Support Vector Machine   ##########################
##########################################################################
# SVM : radial kernel

library(e1071)
mdl_svm <- svm(Survived ~ Pclass + Sex + SibSp+Parch+Fare, data = train, kernel = "sigmoid") ## sigmoid kernel
mdl_svm

str(test)
pred <- predict(mdl_svm,newdata = test)
pred
out <- data.frame(PassengerId = test$PassengerId, Survived = pred)
head(out)

#########################################################################
### Mean square prediction error
#out$Survived <- ifelse(out$Survived == 2,1,0)
actual.out <- data.frame(PassengerID = test$PassengerId, Survived = test$Survived)
MSPE = mean((out$Survived-actual.out$Survived)^2)
cat(" Mean Square prediction error is : -> ", MSPE)
#########################################################################

write.csv(out,file="data-cleanup/svm-predict.csv",row.names = FALSE)
#########################################################################
#########################################################################

##########################################################################
##### Prediction using Gradient boosting machine (GBM)  ##################
##########################################################################
library(caret)
library(pROC)
forest.model <- train(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID, train, importance=TRUE)
## 10-fold CV
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
gbm.model <- train(Survived ~ Pclass + Sex + SibSp +Parch , train, distribution = "gaussian", method = "gbm", 
                   importance=TRUE, trControl = fitControl,verbose = FALSE)
# Look at variable importance

#
# prediction
#predict<- predict(forest.model , test , type="prob")
predict<- predict(forest.model , test )
out <- data.frame(PassengerId = test$PassengerId, Survived = predict)
head(out)
out$Survived <- ifelse(out$Survived == 2,1,0)
actual.out <- data.frame(PassengerID = test$PassengerId, Survived = test$Survived)
MSPE = mean((out$Survived-actual.out$Survived)^2)
cat(" Mean Square prediction error is : -> ", MSPE)
### Plot ROC curve.
result.roc.model1 <-  roc(test$Survived, predict)

plot(result.roc.model1, print.thres="best", print.thres.best.method="closest.topleft")

result.coords.model1 <- coords(  result.roc.model1, "best", best.method="closest.topleft",
                                 ret=c("threshold", "accuracy"))
result.coords.model1


#########################################################################
#########################################################################

