library(rpart)
library(e1071)
library(kknn)
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
CrossValidation <- makeResampleDesc(method = "CV", iters = 3)

#Tell mlr what prediction algorithm we'll be using 
#For each algorithm, add predict.type = “response” as an additional argument to the makeLearner() function.
predict.trees <- makeLearner("classif.rpart", predict.type = "response")
predict.logit <- makeLearner("classif.glmnet", predict.type = "response")
predict.nn <- makeLearner("classif.nnet", predict.type = "response")
predict.nb <- makeLearner("classif.naiveBayes", predict.type = "response")
predict.knn <-makeLearner("classif.knn", predict.type = "response")
predict.svm <- makeLearner("classif.svm", predict.type = "response")

#The tuning strategy (e.g. random with 10 guesses)
#Search over penalty parameter lambda and force elastic net parameter to be 1 (LASSO)
#modelParams <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),makeNumericParam("alpha",lower=1,upper=1))

#Take 10 random guesses at lambda
tuneMethod <- makeTuneControlRandom(maxit = 10L)

#Do the tuning
tunedModel <- tuneParams(learner = predAlg,
                         task = task.highearner,
                         resampling = resampleStrat,
                         measures = rmse,      
                         par.set = modelParams,
                         control = tuneMethod,
                         show.info = TRUE)



#Question 6 
#Setting the hyperparameters of each algorithm that will need to be cross validated

#Tree model
#minsplit, which is an integer ranging from 10 to 50 (governs minimum sample size for making a split)
#minbucket, which is an integer ranging from 5 to 50 (governs minimum sample size in a leaf)
#cp, which is a real number ranging from 0.001 to 0.2 (governs complexity of the tree)
hyperparameters.tree <- 



#Logit model 
#(this is the elastic net model from last problem set, so the two parameters are λ and α. 
#For this problem set, let λ ∈ [0, 3] and α ∈ [0, 1].
  
logitmodelParams <- makeParamSet(makeNumericParam("lambda",lower=0,upper=3),makeNumericParam("alpha",lower=0,upper=1))

hyperparameters.logit <-

 
#Neural network model
#size, which is an integer ranging from 1 to 10 (governs number of units in hidden layer)

#decay, which is a real number ranging from 0.1 to 0.5 (acts like λ in the elastic net model)

#maxit, which is an integer ranging from 1000 to 1000
#(i.e. this should be fixed at 1,000 ... it governs the number of iterations the neural network takes when figuring out convergence)

hyperparameters.nn <-


#Naive Bayes
#There’s nothing to regularize here, so you don’t need to do any cross-validation or tuning for this model


#KNN
#k, which is an integer ranging from 1 to 30 
#(governs the number of “neighbors” to consider)
hyperparameters.knn <-



#SVM
#kernel, which is a string value equal to “radial” (program it as “discrete” in mlr’s interface) in the set)
#(governs how soft the margin of classification is)

#cost, which is a real number (“discrete” in mlr’s interface) in the set 
#(governs how soft the margin of classification is) 

#gamma, which is also a real number in the set)
#(governs the shape [variance] of the Gaussian kernel)
hyperparameters.svm <- 







#Question 7 
#Now tune the models. Use the F1 score and the G-measure as criteria for tuning
#(these are f1 and gmean, respectively). Remember that you don’t need to tune the Naive Bayes model.


#Search over penalty parameter lambda and force elastic net parameter to be 1 (LASSO)
modelParams <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),makeNumericParam("alpha",lower=1,upper=1))

#Take 10 random guesses at lambda
tuneMethod <- makeTuneControlRandom(maxit = 10L)

#Do the tuning
tunedModel <- tuneParams(learner = predAlg,
                         task = theTask,
                         resampling = resampleStrat,
                         # RMSE performance measure, this can be changed to one or many
                         measures = rmse,      
                         par.set = modelParams,
                         control = tuneMethod,
                         show.info = TRUE)




#Question 8
#Once tuned, apply the optimal tuning parameters to each of the algorithms 
#(again, you don’t need to do this for Naive Bayes since there was no tuning done previously).
#Then train the models, generate predictions, and assess performance.


