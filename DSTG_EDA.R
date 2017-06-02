#load library
library(ggplot2)


setwd('C:/Users/vnayak/Desktop/TechGig/DataScience')
getwd()

# Read csv
traindf<-read.csv('train_data.csv')
traindf$Credit_History

str(traindf)

# We will plot a histogram of Applicant income and look at its distribution.
ggplot(aes(x=ApplicantIncome),data = traindf)+
  geom_histogram(binwidth =100)+
  scale_x_continuous(limits = c(1000,8000),breaks = seq(1000,80000,1000))


#Histogram to show distribution of Credit_History

ggplot(aes(x=Credit_History),data = traindf)+
  geom_histogram(binwidth =0.5)



#Find any correlation between Applicant income and credit history
cor.test(traindf$ApplicantIncome,traindf$Credit_History)

#Scatterplot for bi-variate analysis between Applicant Income and Loan Amount
ggplot(aes(x=ApplicantIncome,y=LoanAmount),data = traindf)+
  geom_point()+
  geom_smooth(method = 'lm',color='red')

#Find correlation between Applicant income and Loan Amount.
cor.test(traindf$ApplicantIncome,traindf$LoanAmount)
