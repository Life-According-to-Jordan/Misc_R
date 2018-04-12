#load packages
library(mice)
library(lattice)
library(VIM)

#preview data
data<-airquality
head(data)
names(data)

#removing month and day
data <- data[-c(5,6)]

#summary
summary(data)

#missing data pattern
md.pattern(data)

#looking for missing > 5% variables
pMiss <- function(x){sum(is.na(x))/length(x)*100}

#check each column
apply(data,2,pMiss)

#check each row
apply(data,1,pMiss)

# Plot of missing data pattern
aggr_plot <- aggr(data, 
                  col=c('navyblue','red'), 
                  numbers=TRUE, 
                  sortVars=TRUE, 
                  labels=names(data), 
                  cex.axis=1, 
                  gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))

################
#MICE Imputation
################

#pmm = predictive mean matching
data2 <- mice(data,m=5,maxit=50,meth='pmm',seed=500)

#impute data 
mice_data <- complete(data2,1)

#summary data 
summary(mice_data)

#plot data 
#magenta is imputed 
#blue is original data
xyplot(data2,Ozone ~ Wind+Temp+Solar.R,pch=18,cex=1)
