library(rpart)
library(e1071)
library(kknn)
library(class)
library(nnet)
library(mlr)

print("The goal of this problem set is to compare the 5 Tribes of Machine Learning in terms of their ability
to classify whether someone is high-income or not. Thus, the target variable for this exercise will be income$high.earner.")

#set seed for repreducability 
set.seed(100)

#load data 
income <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data")

#set names for data set 
names(income) <- c("age","workclass","fnlwgt","education","education.num","marital.status","occupation","relationship","race","sex","capital.gain","capital.loss","hours","native.country","high.earner")

#check names of income 
print("variables for the data:")
print(names(income))

#Background information of variables 
# From UC Irvine's website (http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names)
#   age: continuous.
#   workclass: Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked.
#   fnlwgt: continuous.
#   education: Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool.
#   education-num: continuous.
#   marital-status: Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse.
#   occupation: Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces.
#   relationship: Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried.
#   race: White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black.
#   sex: Female, Male.
#   capital-gain: continuous.
#   capital-loss: continuous.
#   hours-per-week: continuous.
#   native-country: United-States, Cambodia, England, Puerto-Rico, Canada, Germany, Outlying-US(Guam-USVI-etc), India, Japan, Greece, South, China, Cuba, Iran, Honduras, Philippines, Italy, Poland, Jamaica, Vietnam, Mexico, Portugal, Ireland, France, Dominican-Republic, Laos, Ecuador, Taiwan, Haiti, Columbia, Hungary, Guatemala, Nicaragua, Scotland, Thailand, Yugoslavia, El-Salvador, Trinadad&Tobago, Peru, Hong, Holand-Netherlands.


# Clean up the data
# Drop unnecessary columns
income$native.country <- NULL
income$fnlwgt         <- NULL

# Make sure continuous variables are stored as numeric values 
income$age            <- as.numeric(income$age)
income$hours          <- as.numeric(income$hours)
income$education.num  <- as.numeric(income$education.num)
income$capital.gain   <- as.numeric(income$capital.gain)
income$capital.loss   <- as.numeric(income$capital.loss)

# Combine levels of categorical variables that currently have too many levels
levels(income$education) <- list(Advanced = c("Masters,","Doctorate,","Prof-school,"), Bachelors = c("Bachelors,"), "Some-college" = c("Some-college,","Assoc-acdm,","Assoc-voc,"), "HS-grad" = c("HS-grad,","12th,"), "HS-drop" = c("11th,","9th,","7th-8th,","1st-4th,","10th,","5th-6th,","Preschool,"))
levels(income$marital.status) <- list(Married = c("Married-civ-spouse,","Married-spouse-absent,","Married-AF-spouse,"), Divorced = c("Divorced,","Separated,"), Widowed = c("Widowed,"), "Never-married" = c("Never-married,"))
levels(income$race) <- list(White = c("White,"), Black = c("Black,"), Asian = c("Asian-Pac-Islander,"), Other = c("Other,","Amer-Indian-Eskimo,"))
levels(income$workclass) <- list(Private = c("Private,"), "Self-emp" = c("Self-emp-not-inc,","Self-emp-inc,"), Gov = c("Federal-gov,","Local-gov,","State-gov,"), Other = c("Without-pay,","Never-worked,","?,"))
levels(income$occupation) <- list("Blue-collar" = c("?,","Craft-repair,","Farming-fishing,","Handlers-cleaners,","Machine-op-inspct,","Transport-moving,"), "White-collar" = c("Adm-clerical,","Exec-managerial,","Prof-specialty,","Sales,","Tech-support,"), Services = c("Armed-Forces,","Other-service,","Priv-house-serv,","Protective-serv,"))

# Break up the data:
n <- nrow(income)
train <- sample(n, size = .8*n)
test  <- setdiff(1:n, train)
income.train <- income[train,]
income.test  <- income[test, ]

#Question 5 

# Define the task:
task.highearner <- makeClassifTask(data = income.train, target = "high.earner")
print(task.highearner)

#Set resampling strategy (here let's do 3-fold CV)
resampleStrat <- makeResampleDesc(method = "CV", iters = 3)

#Take 10 random guesses at lambda
tuneMethod <- makeTuneControlRandom(maxit = 10L)

#Tell mlr what prediction algorithm we'll be using 
trees <- makeLearner("classif.rpart", predict.type = "response")
logit <- makeLearner("classif.glmnet", predict.type = "response")
nn <- makeLearner("classif.nnet", predict.type = "response")
nb <- makeLearner("classif.naiveBayes", predict.type = "response")
knn <-makeLearner("classif.kknn", predict.type = "response")
svm <- makeLearner("classif.svm", predict.type = "response")


#Question 6 
#Setting the hyperparameters of each algorithm that will need to be cross validated

#Tree model
parameters.tree <- makeParamSet(
  makeIntegerParam("minsplit", lower = 10, upper = 50),
  makeIntegerParam("minbucket", lower = 5, upper = 50),
  makeNumericParam("cp", lower = 0.001, upper = 0.2))

#Logit model 
parameters.logit <- makeParamSet(
   makeNumericParam("lambda",lower=0,upper=3),
   makeNumericParam("alpha",lower=0,upper=1))

#Neural network model
parameters.nn <- makeParamSet(
  makeIntegerParam("size", lower = 1, upper = 10),
  makeNumericParam("decay", lower = 0.1, upper = 0.5),
  makeIntegerParam("maxit", lower = 1000, upper = 1000))

#Naive Bayes
#There’s nothing to regularize here, so you don’t need to do any cross-validation or tuning for this model!

#KNN
parameters.knn <- makeParamSet(
  makeIntegerParam("k", lower = 1, upper = 30))

#SVM
parameters.svm  <- makeParamSet(
  makeDiscreteParam("cost", values = 2^c(-2, -1, 0, 1, 2, 10)), 
  makeDiscreteParam("gamma", values = 2^c(-2, -1, 0, 1, 2, 10)))

#Question 7 
#I love your singing voice, but you're a little flat, so let's try tunning

#tuning predict.trees
tunedModel.trees <- tuneParams(learner = trees,
                         task = task.highearner,
                         resampling = resampleStrat,
                         measures = list(f1, gmean),
                         par.set = parameters.tree,
                         control = tuneMethod,
                         show.info = TRUE)
#tuning predict.logit
tunedModel.logit <- tuneParams(learner = logit,
                         task = task.highearner,
                         resampling = resampleStrat,
                         measures = list(f1, gmean),
                         par.set = parameters.logit,
                         control = tuneMethod,
                         show.info = TRUE)
#tuning predict.nn
tunedModel.nn <- tuneParams(learner = nn,
                         task = task.highearner,
                         resampling = resampleStrat,
                         measures = list(f1, gmean),      
                         par.set = parameters.nn,
                         control = tuneMethod,
                         show.info = TRUE)

#tuning predict.knn
tunedModel.knn <- tuneParams(learner = knn,
                         task = task.highearner,
                         resampling = resampleStrat,
                         measures = list(f1, gmean),      
                         par.set = parameters.knn,
                         control = tuneMethod,
                         show.info = TRUE)
#tuning predict.svm
tunedModel.svm <- tuneParams(learner = svm,
                         task = task.highearner,
                         resampling = resampleStrat,
                         measures = list(f1, gmean),      
                         par.set = parameters.svm,
                         control = tuneMethod,
                         show.info = TRUE)

#Question 8
#Once tuned, apply the optimal tuning parameters to each of the algorithms 
#There's no need to do this for Naive Bayes since there was no tuning done previously


#stop! training time! 

#apply the optimal algorithm parameters to the model
pred.trees <- setHyperPars(learner=trees, par.vals = tunedModel.trees$x)
pred.logit <- setHyperPars(learner=logit, par.vals = tunedModel.logit$x)
pred.nn    <- setHyperPars(learner=nn, par.vals = tunedModel.nn$x)
pred.knn   <- setHyperPars(learner=knn, par.vals = tunedModel.knn$x)
pred.svm   <- setHyperPars(learner=svm, par.vals = tunedModel.svm$x)

#verify performance on cross validated sample sets
sampleResults.tree  <- resample(learner = pred.trees, task = task.highearner, resampling = resampleStrat, measures=list(gmean))
sampleResults.logit <- resample(learner = pred.logit, task = task.highearner, resampling = resampleStrat, measures=list(gmean))
sampleResults.nn    <- resample(learner = pred.nn, task = task.highearner, resampling = resampleStrat, measures=list(gmean))
sampleResults.knn   <- resample(learner = pred.knn, task = task.highearner, resampling = resampleStrat, measures=list(gmean))
sampleResults.svm   <- resample(learner = pred.svm, task = task.highearner, resampling = resampleStrat, measures=list(gmean))

#run model on training data 
finalModel.tree  <- train(learner = pred.trees, task = task.highearner)
finalModel.logit <- train(learner = pred.logit, task = task.highearner)
finalModel.nn    <- train(learner = pred.nn, task = task.highearner)
finalModel.knn   <- train(learner = pred.knn, task = task.highearner)
finalModel.nb    <- train(learner = nb, task = task.highearner)
finalModel.svm   <- train(learner = pred.svm, task = task.highearner)

# Predict in test set for each algorithm
prediction.test.tree  <- predict(finalModel.tree, newdata = income.test)
prediction.test.logit <- predict(finalModel.logit, newdata = income.test)
prediction.test.nn    <- predict(finalModel.nn, newdata = income.test)
prediction.test.knn   <- predict(finalModel.knn, newdata = income.test)
prediction.test.nb    <- predict(finalModel.nb, newdata = income.train)
prediction.test.svm   <- predict(finalModel.svm, newdata = income.test)

# Out of sample f1 and gmean for each algorithm
performance(prediction.test.tree, measures = list(f1, gmean))
performance(prediction.test.logit, measures = list(f1, gmean))
performance(prediction.test.nn, measures = list(f1, gmean))
performance(prediction.test.knn, measures = list(f1, gmean))
performance(prediction.test.nb, measures = list(f1, gmean))
performance(prediction.test.svm, measures = list(f1, gmean))

