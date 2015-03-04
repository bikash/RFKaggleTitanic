#################################################################################################
#################################################################################################
## Author: Bikash Agrawal
## Date: 24th Feb 2015
## Email: er.bikash21@gmail.com
## Description: Apply Random forest to predict the survival rate and create a new csv file. This algorithm might helps to be in top 10 in Kaggle competition.
##              Kaggle titanic competition in kaggle.com.
##              http://www.kaggle.com/c/titanic-gettingStarted
## Step 1:  Data clean up
## Step 2:  Calculate probability of survival using Condition Inference Random Forest.
## References: 
## [1] http://nbviewer.ipython.org/github/agconti/kaggle-titanic/blob/master/Titanic.ipynb
## [2] http://www.philippeadjiman.com/blog/category/machine-learning/
## [3] http://rforwork.info/tag/linear-discriminant-analysis/
## source("/Users/bikash/repos/RFKaggleTitanic/R/CIRF.R")
#################################################################################################
#################################################################################################

### setting path of repo folder.
getwd()
setwd("/Users/bikash/repos/RFKaggleTitanic/")

library(dplyr)
library(zoo)
library(randomForest)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(lattice)
library(caret)
library(party)
library(Amelia) ## Amelia is packages to display missing data using missmap function
library(corrgram) ## display corrgram plot of different varaibles
sessionInfo()

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
##    Min. 1st Qu.  Median    Mean  3rd Qu.    Max.    NA's 
##   0.17   21.00   28.00   29.88     39.00   80.00     263 
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

# All the missing cabin
summary(combi$Cabin) ## check if fare is missing NA's ->1
which(is.na(combi$Cabin)) ## get id for missing data

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


## Display missing data from training data sets
missmap(combi, main="Titanic Training Data - Missings Map", 
        col=c("yellow", "black"), legend=FALSE)


##########################################################################
##### Display plot of different variables                 ################
##########################################################################
pdf("graphs/plot.pdf",bg="white")
barplot(table(train$Survived),names.arg = c("Dead", "Survived"), main="Survived (passenger)", col="blue")
barplot(table(combi$Pclass), names.arg = c("first", "second", "third"), main="Pclass (passenger traveling class)", col="firebrick")
barplot(table(combi$Sex), main="Sex (gender)", col="darkviolet")
hist(combi$Age, main="Age", xlab = NULL, col="brown")
barplot(table(combi$SibSp), main="SibSp (siblings + spouse aboard)", col="darkblue")
barplot(table(combi$Parch), main="Parch (parents + kids aboard)", col="gray50")
hist(combi$Fare, main="Fare (fee paid for ticket[s])", xlab = NULL,  col="darkgreen")
barplot(table(combi$Embarked), names.arg = c("Cherbourg", "Queenstown", "Southampton"),main="Embarked (port of embarkation)", col="sienna")

boxplot(combi$Age ~ combi$Pclass, main="Passenger by Class",  xlab="PClass", ylab="Age") ##relationship between Age and PClass
boxplot(df_train$Age ~ df_train$Survived, main="Passenger by Age", xlab="Survived", ylab="Age") ## relationship between Age and Survival


## corrgram plot
corrgram.data <- df_train
## change features of factor type to numeric type for inclusion on correlogram
corrgram.data$Survived <- as.numeric(corrgram.data$Survived)
corrgram.data$Pclass <- as.numeric(corrgram.data$Pclass)
corrgram.data$Embarked <- revalue(corrgram.data$Embarked, c("C" = 1, "Q" = 2, "S" = 3))
## generate correlogram
corrgram.vars <- c("Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")
corrgram(corrgram.data[,corrgram.vars], order=FALSE, lower.panel=panel.ellipse, upper.panel=panel.pie, text.panel=panel.txt, main="Titanic Training Data")
corrgram(corrgram.data[,corrgram.vars], order=TRUE, lower.panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt, main="Titanic Training Data")
dev.off()


##########################################################################
##### Prediction using Condition Inference Random Forest  ################
##########################################################################
print("Prediction using Condition Inference Random Forest......")
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Ticket+ Fare + Embarked + Title + FamilySize + FamilyID2 + Pclass:Sex + Pclass:Age + Age:Sex,
               data = train, controls=cforest_unbiased(ntree=1000, mtry=3))


## Tree structure
fit = ctree(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Cabin + Parch + Fare + Embarked + Title + FamilySize + FamilyID2 + Pclass:Sex + Pclass:Age + Age:Sex,
            data = train,   
            controls = ctree_control(
              teststat="quad",
              testtype="Univariate",
              mincriterion=.95,
              minsplit=10, 
              minbucket=5,
              maxdepth=0
            ))
plot(fit)

fancyRpartPlot(fit)

#prediction
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
prop.table(table(test$Survived, Prediction),1)
## calculate accuracy of model
accuracy = sum(Prediction==test$Survived)/length(Prediction)
print (sprintf("Accuracy = %3.2f %%",accuracy*100)) ### 82.84% accuracy of model
#########################################################################
out <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(out, file = "data-cleanup/ciRandomForest-predict1.csv", row.names = FALSE)
#########################################################################
#########################################################################