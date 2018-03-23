#Load data 
data(mtcars)

#view data 
mtcars

#we do not have to clean our data since it is a preloaded dataset in R 

#view all variables and their classes in data set
sapply(mtcars, class)

#generate model
#regress vs on weight and displacement
model<-glm(formula=vs~wt+disp, data = mtcars, family = "binomial")
model

#summary of model 
summary(model)

#Null deviance: 43.86  on 31  degrees of freedom
#null deviance is the model with no features, thus df = 31

#Residual deviance: 21.40  on 29  degrees of freedom
#Residual deviance with our 2 independent variables is lower than with no features, also df = 31 - 2 (features) = 29 

#the lower the deviance, the better our model 

#store new data
newdata<-data.frame(wt=2.1, disp=180)

#prediction for new data
predict(model, newdata,type="response")

#given the probability is relatively low (0.2361081), the vehicle is unlikely to be classified as a "vs"

#expanding our model 
model<-glm(formula=vs~mpg+cyl+disp+hp+drat+wt+qsec+am+gear+carb, data = mtcars, family = "binomial")

#store new data
newdata2<-data.frame(mpg=22, cyl = 4, disp = 110, hp = 93, drat = 4, wt=2.5, qsec = 20, am = 1, gear = 4, carb =1)

predict(model, newdata2,type="response")

#results are 1, thus we are 100% sure that the car whose features were mentioned above is a vs type car 
