'''
Goal of this analysis is to determine if Nick Saban is overpaid 

We will only analyze his performance on the field to determine if his salary is comparable
'''
# Original Author :   Prof. Le Wang
# Thank you Dr. Wang for providing a working framework to build off of 

#this program is to illustrate the use of simple linear regression
#when the predictor is a qualitative variable.

#set working directory
setwd("your_directory_here")

#load data 
mydata <- read.table("coach_salary_usatoday02.csv", header=TRUE, sep=",")

#only use other coaches salary
others <- mydata[2:10,]

#run the model
fit<-lm(total ~ share + career + sec, data=others)
summary(fit)

#share of loss = 0
#career wins = 167
#sec=1 

#saban's salary less what we expect to see as a comparable salary
5545852 - predict(fit, newdata = data.frame(share=0,career=167,sec=1))

#comparable salaries are making 414423.3 
#thus Saban is overpaid

#run another model with interaction term this time
fit2<-lm(total ~ share + career + sec + share*career, data=others)
summary(fit2)

predict(fit2, newdata = data.frame(share=0,career=167,sec=1))

#saban's salary less what we expect to see as a comparable salary
5545852 - predict(fit2, newdata = data.frame(share=0,career=167,sec=1))
#the error we're making is 43705.76

(5545852 - predict(fit2, newdata = data.frame(share=0,career=167,sec=1)))/5545852
#0.007880801 
#the error we're making is less than 1% 


#next we need to determine if the other coaches are overpaid 
#linear regression may not be the best model for this analysis 
#perhaps a conditional probability would be a better fit to determine fair salary

#next we need to determine whether or not we want to keep saban in as we evaluate 
#other salary's as his salary may unneccesarily bringe up their expected wage 



