#sample data 
set.seed(123456)
y<-sample(c(1:4), 1000, replace = TRUE, prob = c(0.25, 0.25, 0.25, 0.25))
x<-sample(c(0:1), 1000, replace = TRUE, prob = c(0.5, 0.5))
z<-sample(c(0:1), 1000, replace = TRUE, prob = c(0.25, 0.75))

#make data frame 
data.xyz <- as.data.frame(cbind(x,y,z))

#view top of data 
head(data.xyz)

#Subsetting
prop.table(table(y[x==0]))
prop.table(table(y[x==1]))

#Alternative way to subset data 
data.xo<-subset(data.xyz, x==0, select=y)
prop.table(table(data.xo))

data.x1<-subset(data.xyz, x==1, select=y)
prop.table(table(data.x1))

#Aggregate Function 
#Pr[Y = y] = E[I(Y = y)]
#Pr[Y = y| X = x] = E[I(Y = y)|X = x]

#can omit 0, and this would replace dummy.factor 1 
#you would wnat to omit this to limit perfect collinearity 
data.xyz$dummies <- model.matrix(~factor(y) + 0, data = data.xyz)
head(data.xyz)

#aggregate automatically calculates mean for each x value 
aggregate(data.xyz$dummies, by=list(x), FUN=mean)

#column restraint for x = 0/1, you may want to restrict the information shown after running the script 
aggregate(data.xyz$dummies, by=list(x), FUN=mean)[2:5]

#conditional distribution given 2 variables 
aggregate(data.xyz$dummies, by=list(x,z), FUN=mean)
