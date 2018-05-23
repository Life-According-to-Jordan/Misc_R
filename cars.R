#load cars data set from R 
#this data set is relatively small and consists of the stopping times of cars given their distance

#load data 
data <- cars

#summary stats
summary(data)

#get col names 
names(data)

#regress stop time on distance
results<-lm(data$speed~data$dist)

#summary of regression 
summary(results)

#plot results 
plot(results)

#stopping distance follows a linear trend 
#the faster the car, the greater the distance traveled before stopping
#discrepencies in the data and results could stem from different models of cars 
#as well as being a truck, sedan, suv, etc. 


