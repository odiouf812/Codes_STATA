# Random Forest in R

### Random Forest in R, Random forest developed by an aggregating tree and this can be used for classification and regression. One of the major advantages is its avoids overfitting.

The random forest can deal with a large number of features and it helps to identify the important attributes.

The random forest contains two user-friendly parameters ntree and mtry.

ntree- ntree by default is 500 trees.

mtry- variables randomly samples as candidates at each split.

#### Random Forest Steps

1.Draw ntree bootstrap samples.

2.For each bootstrap, grow an un-pruned tree by choosing the best split based on a random sample of mtry predictors at each node

3.Predict new data using majority votes for classification and average for regression based on ntree trees.

# Load Library

library(randomForest)
library(datasets)
library(caret)

# Getting Data

data<-iris
str(data)

The datasets contain 150 observations and 5 variables. Species considered as response variables. Species variable should be a factor variable.

data$Species <- as.factor(data$Species)
table(data$Species)
setosa versicolor  virginica
 50         50         50

# Data Partition

Lets start with random seed so the outcome will be repeatable and store train and test data.

set.seed(222)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]

106 observations in train data set and 44 observatons in test data.

# Random Forest in R

rf <- randomForest(Species~., data=train, proximity=TRUE) print(rf)
Call:
 randomForest(formula = Species ~ ., data = train)
               Type of random forest: classification
                     Number of trees: 500
No. of variables tried at each split: 2
        OOB estimate of  error rate: 2.83%
# Confusion matrix:

           setosa versicolor virginica class.error
setosa         35          0         0  0.00000000
versicolor      0         35         1  0.02777778
virginica       0          2        33  0.05714286

# Prediction & Confusion Matrix – train data

p1 <- predict(rf, train)
confusionMatrix(p1, train$ Species)

# Prediction & Confusion Matrix – test data

p2 <- predict(rf, test)
confusionMatrix(p2, test$ Species)
Confusion Matrix and Statistics

# Error rate of Random Forest

plot(rf)

# Tune mtry

t <- tuneRF(train[,-5], train[,5],
       stepFactor = 0.5,
       plot = TRUE,
       ntreeTry = 150,
       trace = TRUE,
       improve = 0.05)

# No. of nodes for the trees

hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "green")
Variable Importance
varImpPlot(rf,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")
importance(rf)
MeanDecreaseGini

# Partial Dependence Plot

partialPlot(rf, train, Petal.Width, "setosa")

# Multi-dimensional Scaling Plot of Proximity Matrix

MDSplot(rf, train$Species)

