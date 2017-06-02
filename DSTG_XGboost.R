library('xgboost')
library(readr)
library(stringr)
library(caret)
require(Matrix)
require(data.table)



#set and get working directory
setwd('C:/Users/vnayak/Desktop/TechGig/DataScience')
getwd()

# Read csv
traindf<-read.csv('train_data.csv')
test_datadf<-read.csv('test_data.csv')
test.ApplicationID<-test_datadf$Application_ID

head(traindf)
head(test_datadf)

str(traindf)
str(test_datadf)


#Find missing values
sum(is.na(traindf))
sum(is.na(test_datadf))

#Imputing missing values using KNN.Also centering and scaling numerical columns
preProcValues <- preProcess(traindf, method = c("knnImpute","center","scale"))
preProcValues_testdatadf <- preProcess(test_datadf, method = c("knnImpute","center","scale"))


train_processed <- predict(preProcValues, traindf)
test_datadf_processed<-predict(preProcValues_testdatadf, test_datadf)

sum(is.na(train_processed))
sum(is.na(test_datadf_processed))


#Convert outcome variable to numeric.
train_processed$Loan_Status<-ifelse(train_processed$Loan_Status=='N',0,1)
train_processed$Loan_Status

#Removing ID variable
loanAppid<-train_processed$Application_ID


train_processed$Application_ID<-NULL
test_datadf_processed$Application_ID<-NULL

str(train_processed)
str(test_datadf_processed)


#Converting every categorical variable to numerical using dummy variables
dmy <- dummyVars(" ~ .", data = train_processed,fullRank = T)
train_transformed <- data.frame(predict(dmy, newdata = train_processed))

dmy_testdf <- dummyVars(" ~ .", data = test_datadf_processed,fullRank = T)
test_transformed <- data.frame(predict(dmy_testdf, newdata = test_datadf_processed))


str(train_transformed)
str(test_transformed)


#Converting the dependent variable back to categorical
train_transformed$Loan_Status<-as.factor(train_transformed$Loan_Status)

str(train_transformed)

#Keep variables which are important for prediction
train_transformed
train_transformed<-train_transformed[c("Credit_History","LoanAmount","ApplicantIncome",
                      "Married.Yes","Loan_Status")]

test_transformed<-test_transformed[c("Credit_History","LoanAmount","ApplicantIncome",
                                     "Married.Yes")]



target<-train_transformed$Loan_Status
train_transformed$Loan_Status<-NULL


set.seed(40)
model_xgb_cv <- xgb.cv(data=as.matrix(train_transformed), label=as.matrix(target), objective="binary:logistic", nfold=5, 
                       nrounds=16, eta=0.01, max_depth=6, subsample=0.7, colsample_bytree=0.85, 
                       min_child_weight=1)


# model building

set.seed(40)
model_xgb <- xgboost(data=as.matrix(train_transformed), label=as.matrix(target), objective="binary:logistic", 
                     nrounds=16, eta=0.01, max_depth=6, subsample=0.7, 
                     colsample_bytree=0.85, min_child_weight=1)

#######  start  #############
pred <- predict(model_xgb, as.matrix(test_transformed))
prediction = as.numeric(pred > 0.5)
prediction

names <- dimnames(as.matrix(train_transformed))[[2]]
# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = model_xgb)
xgb.plot.importance(importance_matrix[1:5,])


submitdf<-read.csv('submission.csv')
head(submitdf)

submitdf$Loan_Status
submitdf$Application_ID<-test.ApplicationID
submitdf$Loan_Status<-prediction
submitdf$Loan_Status<-ifelse(submitdf$Loan_Status==1,'Y','N')
write.csv(submitdf,file ="upload3.csv",row.names = FALSE)

##############end #################






