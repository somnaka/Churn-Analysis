#-----Section 1----------------
# set working directory
setwd(dirname(file.choose()))
getwd()

# read in data from csv file
mydata <- read.csv("Churn_modelling.csv", stringsAsFactors = FALSE)
head(mydata)    # Inspect top rows of the data
str(mydata)
#---------Section 2---------------------
#Data Preprocessing

#Label Encoding
mydata$Gender <- as.numeric(factor(mydata$Gender))
mydata$Geography <- as.numeric(factor(mydata$Geography))

str(mydata)

# check for missing data

apply(mydata, MARGIN = 2, FUN = function(x) sum(is.na(x)))
library(Amelia)
missmap(mydata, col = c("black", "lightblue"), legend = TRUE)

#---------------------SECTION 3-------------------------
# DATA EXPLORATION

# Mean, Std. deviation, Maximum and Minimum of "Exited"
summary(mydata$Exited)
summary(mydata)

#Dropping unnecessary columns
mydata <- mydata[, -c(1, 2, 3)]
str(mydata)

# Create a boxplot of all independent/dependent variables
boxplot(mydata)
boxplot(mydata$Exited)
attach(mydata)


# frequency histogram
hist(mydata$Exited)
hist(mydata$Exited, col = "light blue", border = "dark blue", freq = T, ylim = c(0,10000),
     xlab = "Percentage Churn", main = "Histogram")


# Plot histogram and normal approximation
library(rcompanion)
plotNormalHistogram(Exited, main = "Histogram", xlab = "Exited")

# KS test- test for normality

ks.test(Exited, "pnorm", mean(Exited), sd(Exited))


#--------------------Section 4-------------------
#________DATA ANALYSIS_____
#SCALING Dataset

#Min/Max scaling
data2 <- mydata[, -11]

head(data2)
data.mm <- apply(data2, MARGIN = 2, FUN = function(x) scale(x, center = TRUE, scale = TRUE))
data.mm <- apply(data.mm, MARGIN = 2, FUN = function(x) scale(x, center = TRUE, scale = TRUE))

head(data.mm)
boxplot(data.mm)

#change matrix to a dataframe
df <- as.data.frame(data.mm)

# bring back the removed column
df <- cbind(df, Exited = mydata$Exited)
head(df)

# usage of scale() Zscore
#scaled_data <- scale(mydata)
#boxplot(scaled_data)
#head(scaled_data)

#------------------------Correlation test techniques----------------------
# using subset of data
mydata2 <- data.frame(CreditScore,	Geography,	Gender,	Age,	Tenure,	Balance,	NumOfProducts,	HasCrCard,	IsActiveMember,	EstimatedSalary,	Exited
)
# add column names
colnames(mydata2) <- c(CreditScore,	Geography,	Gender,	Age,	Tenure,	Balance,	NumOfProducts,	HasCrCard,	IsActiveMember,	EstimatedSalary,	Exited
)
# simple correlation matrix
cor(mydata2, method = "spearman")
mydata_cor <- cor(mydata2, method = "spearman")

round(mydata_cor, digits = 2)
#----------------Correlation plots-----------------------
# Using Corrplot
library(corrplot)
mydata2 <- data.frame(CreditScore,	Geography, Gender,	Age,	Tenure,	Balance,	NumOfProducts,	HasCrCard,	IsActiveMember,	EstimatedSalary,	Exited
)
cor_matrix <- cor(mydata2)

# Plot the correlation matrix using corrplot
corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45)

# Using Corrgram

cor.matrix <- cor(mydata2, use = "pairwise.complete.obs", method = "spearman")
round(cor.matrix, digits = 2)
cor.df <- as.data.frame(cor.matrix)
View(cor.df)

library(psych)

pairs.panels(mydata2, method = "spearman", hist.col = "grey", col = "blue", main = "Spearman")

library(corrgram)
# corrgram works best with Pearson correlation
corrgram(mydata2, order=FALSE, cor.method = "pearson", lower.panel=panel.conf,
         upper.panel=panel.pie, text.panel=panel.txt, main="London variables")


# Using Dataset without the unnecessary columns

# Kaiser-Meyer-Olkin statistics: if overall MSA > 0.6, proceed to factor analysis
library(psych)
KMO(cor(mydata))
KMO(cor(df))  #using the result of MIN/MAX scaling
#KMO(cor(scaled_data)) #using data from Zscore scaling

str(df)
mydata <- df #Assigning df back as mydata
str(mydata)

df <- mydata[, -11]
head(df)
# Determine Number of Factors to Extract
library(nFactors)

# get eigenvalues: eigen() uses a correlation matrix
ev <- eigen(cor(mydata))
ev$values
# plot a scree plot of eigenvalues
plot(ev$values, type="b", col="blue", xlab="variables")

# calculate cumulative proportion of eigenvalue and plot
ev.sum<-0
for(i in 1:length(ev$value)){
  ev.sum<-ev.sum+ev$value[i]
}
ev.list1<-1:length(ev$value)
for(i in 1:length(ev$value)){
  ev.list1[i]=ev$value[i]/ev.sum
}
ev.list2<-1:length(ev$value)
ev.list2[1]<-ev.list1[1]
for(i in 2:length(ev$value)){
  ev.list2[i]=ev.list2[i-1]+ev.list1[i]
}
plot (ev.list2, type="b", col="red", xlab="number of components", ylab ="cumulative proportion")

#-----Section 08----

# Varimax Rotated Principal Components
# retaining 'nFactors' components
library(GPArotation)
library(psych)
# principal() uses a data frame or matrix of correlations
fit <- principal(df, nfactors=3, rotate="varimax")
fit


#Change dependent variable to factor

mydata$Exited <- as.factor(mydata$Exited)
str(mydata)

#---------------------SECTION 5----------------------
#____Model Pre-build______

# Randomization
# create training (80%) and test data (20%) (data already in random order)
set.seed(12345)
mydata_rand <- mydata[order(runif(10000)), ]


data_train <- mydata_rand[1:7999, ]
data_test <- mydata_rand[8000:10000, ]

class(data_train) #confirm its a dataframe

# check the proportion of Exited variable
prop.table(table(data_train$Exited))
prop.table(table(data_test$Exited))

# create labels (from first column) for training and test data
data_train_labels <- mydata[1:8000, 1]
data_test_labels <- mydata[8001:10000, 1]  


#----------------2nd Model Random split----------------------------
# create training (70%) and test data (30%) (data already in random order)
set.seed(12345)
mydata_rand <- mydata[order(runif(10000)), ]


data_train <- mydata_rand[1:6999, ]
data_test <- mydata_rand[7000:10000, ]

class(data_train) #confirm its a dataframe

# check the proportion of class variable
prop.table(table(data_train$Exited))
prop.table(table(data_test$Exited))

# create labels (from first column) for training and test data
data_train_labels <- mydata[1:7000, 1]
data_test_labels <- mydata[7001:10000, 1]

#-----------------SECTION 6-----------------------
# ML techniques and Modelling
#----------------Decision Tree-----------
install.packages("C50")
library(C50)
set.seed(12345)

data_model <- C5.0(data_train[-11], data_train$Exited)

# display simple facts about the tree
data_model

# display detailed information about the tree
summary(data_model)

# evaluating model performance

# create a factor vector of predictions on test data
data_pred1 <- predict(data_model, data_test)

# Diagnostics-------------
library(caret)
confusionMatrix(data_pred1, data_test$Exited, mode = "everything", positive = "1") #could be "yes" or "1"

#-------Improving The Model Performance

# pruning the tree to simplify and/or avoid over-fitting
??C5.0Control

set.seed(12345)
data_prune <- C5.0(data_train[-11], data_train$Exited,
                   control = C5.0Control(minCases = 10)) # 1% training obs.

data_prune

summary(data_prune)
data_prune_pred <- predict(data_prune, data_test)

library("gmodels")
CrossTable(data_test$Exited, data_prune_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual Exit', 'predicted Exit'))

confusionMatrix(data_prune_pred, data_test$Exited, mode = "everything", positive = "1")

# boosting the accuracy of decision trees
# boosted decision tree with 10, 20, 100 trials

set.seed(12345)
data_boost10 <- C5.0(data_train[-11], data_train$Exited, control = C5.0Control(minCases = 10), trials = 100)
data_boost10
summary(data_boost10)

data_boost_pred10 <- predict(data_boost10, data_test)

library("gmodels")
CrossTable(data_test$Exited, data_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual Exit', 'predicted Exit'))

confusionMatrix(data_boost_pred10, data_test$Exited, mode = "everything", positive = "1")


#---------------SUPPORT VECTOR MACHINES-------------------

# support vector machine algorithms
library(MASS)
library(DMwR2)
install.packages("polycor")
library(polycor)
install.packages("kernlab")
library(kernlab)
library(caret)

??ksvm()

# run initial model
set.seed(12345)
svm0 <- ksvm(Exited ~ ., data = data_train, kernel = "vanilladot", type = "C-svc")
# vanilladot is a Linear kernel

# basic information about the model
svm0

# Evaluation
data.pred0 <- predict(svm0, data_test)
table(data.pred0, data_test$Exited)
round(prop.table(table(data.pred0, data_test$Exited))*100,1)

# sum diagonal for accuracy
sum(diag(round(prop.table(table(data.pred0, data_test$Exited))*100,1)))

confusionMatrix(data.pred0, data_test$Exited, mode = "everything", positive = "1")

# CHANGING THE KERNEL

set.seed(12345)
svm1 <- ksvm(Exited ~ ., data = data_train, kernel = "rbfdot", type = "C-svc")
# A Gaussian radial base

# basic information about the model
svm1

# evaluation of model
data.pred1 <- predict(svm1, data_test)
table(data.pred1, data_test$Exited)
round(prop.table(table(data.pred1, data_test$Exited))*100,1)

# sum diagonal for accuracy
sum(diag(round(prop.table(table(data.pred1, data_test$Exited))*100,1)))
#less error, more accuracy

confusionMatrix(data.pred1, data_test$Exited, mode = "everything", positive = "1")


#----------------Logistic Regression--------

#First round use all variables

mylogit1 = glm(Exited ~ CreditScore +	Geography +	Gender +	Age +	Tenure +	Balance +	NumOfProducts +	HasCrCard + IsActiveMember +	EstimatedSalary,
               data = data_train, family = "binomial")
summary(mylogit1)

# Calculate Odds Ratio - Exp(b) with 95% confidence intervals (2 tail)
exp(cbind(OR = coef(mylogit1), confint(mylogit1)))


# Second round excluding variable ??

mylogit2 = glm(Exited ~ Geography +	Gender +	Age +	Balance +	IsActiveMember, 
               data = data_train, family = "binomial")
summary(mylogit2)

# Calculate Odds Ratio - Exp(b) with 95% confidence intervals (2 tail)
exp(cbind(OR = coef(mylogit2), confint(mylogit2)))


# Step 1: Make Predictions
predictions <- predict(mylogit1, newdata = data_test, type = "response")

# Step 2: Create Confusion Matrix
# Assuming 'Exited' is the response variable
true_labels <- data_test$Exited
predicted_labels <- ifelse(predictions > 0.5, 1, 0)  # Convert probabilities to binary labels

# Create confusion matrix
conf_matrix <- table(True = true_labels, Predicted = predicted_labels)

# Step 3: Display Confusion Matrix
print(conf_matrix)

accuracy <- mean(predicted_labels == data_test$Exited)

# Print the accuracy
print(accuracy)

#For logit2

# Step 1: Make Predictions
predictions <- predict(mylogit2, newdata = data_test, type = "response")

# Step 2: Create Confusion Matrix
# Assuming 'Exited' is the response variable
true_labels <- data_test$Exited
predicted_labels <- ifelse(predictions > 0.5, 1, 0)  # Convert probabilities to binary labels

# Create confusion matrix
conf_matrix <- table(True = true_labels, Predicted = predicted_labels)

# Step 3: Display Confusion Matrix
print(conf_matrix)

accuracy <- mean(predicted_labels == data_test$Exited)

# Print the accuracy
print(accuracy)

#---------------Random Forest------------------

# random forest with default settings
library(randomForest)

?randomForest()

set.seed(12345)
rf <- randomForest(Exited ~ ., data = data_train)
# summary of model
rf

# variable importance plot
varImpPlot(rf, main = "rf - variable importance")

# apply the model to make predictions
p <- predict(rf, data_test)

# evaluate
CrossTable(data_test$Exited, p,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual Churn', 'predicted Churn'))
confusionMatrix(p, data_test$Exited, positive = "1")

# auto-tune a random forest

ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

grid_rf <- expand.grid(.mtry = c(4, 8, 16, 32))
grid_rf

# warning - takes a long time
set.seed(12345)
rf <- train(Exited ~ ., data = data_train, method = "rf",
            metric = "Kappa", trControl = ctrl,
            tuneGrid = grid_rf)
# summary of model
rf

# apply the model to make predictions
p <- predict(rf, data_test)

# evaluate
CrossTable(data_test$Exited, p,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual Churn', 'predicted Churn'))
confusionMatrix(p, data_test$Exited, positive = "1")


# using the cutoff parameter 

set.seed(12345)
rf <- randomForest(Exited ~ ., data = data_train, nodesize = 10, cutoff = c(.9,.1))
# summary of model
rf

# apply the model to make predictions
p <- predict(rf, data_test)

# variable importance plot
varImpPlot(rf, main = "rf - variable importance")

# evaluate
CrossTable(data_test$Exited, p,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual Churn', 'predicted Churn'))
confusionMatrix(p, data_test$Exited, positive = "1")

detach (mydata)
