#conditional probability and linear regression
#both show the partial impact of marriage
#or in economic terms the marriage premium

#load data 
wage<-read.csv("wage2.csv")

#set bounds for conditional distribution 
#able to see all areas of married, not-married, black, or not-black 
new<-data.frame(married=c(0,1,0,1), black=c(0,0,1,1))

predict(results, new)

#set conditional distribution 
cef<-aggregate(wage, by=list(wage$married, wage$black), FUN=mean)
cef

#OLS regression on data 
results<-lm(wage~married*black, data=wage)

#the difference between the intercept and marriage is the partial impact of marriage 
summary(results)
