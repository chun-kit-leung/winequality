library(corrplot)
library(RColorBrewer)
library(ggfortify)
library(ggplot2)
library(dplyr)
library(class)
library(mlbench)
library(tibble)
library(rpart)
library(randomForest)
library(corrplot)
library(tidyverse)  
library(leaps)
library(tidyverse)
library(caret)
library(e1071)
library(caTools)
library(caret)
library(car)
library("MASS")

set.seed(111)

# load the data
data <-read.csv('winequality-red.csv',header = TRUE)
attach(data) 

# quick check for the data
is.null(data)
dim(data)
head(data) 

hist(data$fixed.acidity)
hist(data$volatile.acidity)
hist(data$citric.acid)
hist(data$residual.sugar)
hist(data$chlorides)
hist(data$free.sulfur.dioxide)
hist(data$total.sulfur.dioxide)
hist(data$density)
hist(data$pH)
hist(data$sulphates)
hist(data$alcohol)
hist(data$quality)

scatterplotMatrix(~ quality + fixed.acidity  +  volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol , data = data)

# check the correlation between diffience features
M <-cor(data)
corrplot(cor(data),
         method = "number",
         type = "upper" )

# PCA analysis

data.pr <- prcomp( data[c(1:11)], center = TRUE, scale = FALSE)
# data.pr <- prcomp( data[c(1:11)], center = TRUE, scale = TRUE)

summary(data.pr)

autoplot(data.pr, data = data, colour = 'quality')

# split training data and testing data

smp_size_raw <- floor(0.75 * nrow(data))
train_ind_raw <- sample(nrow(data), size = smp_size_raw)
train_raw.df <- as.data.frame(data[train_ind_raw, ])
test_raw.df <- as.data.frame(data[-train_ind_raw, ])

attach(train_raw.df)

# use knn to predict the quality of the wine 
data.knn <- knn(train_raw.df[,1:11], test_raw.df[,1:11], train_raw.df[,12], k = 6, l = 0, prob = FALSE, use.all = TRUE)
data.knn

mean(test_raw.df[,12] == data.knn)




train_raw2.df = train_raw.df
train_raw2.df$quality <- as.character(train_raw2.df$quality)
test_raw2.df = test_raw.df
test_raw2.df$quality <- as.character(test_raw2.df$quality)

sum(diag(table(data.knn,test_raw.df[,12])))/nrow(test_raw2.df)
-k 
# use random forest to predict the quality 

data.randomforest <- randomForest(as.factor(quality) ~ .,data = train_raw2.df)

pred <- predict(data.randomforest, newdata = test_raw2.df)

table(pred, test_raw2.df[,12])

plot(data.randomforest)

#the accuracy rate :
sum(diag(table(pred, test_raw2.df[,12])))/nrow(test_raw2.df)



# use linear regression to prediction the quality 


fixed.acidity = train_raw.df$fixed.acidity
volatile.acidity = train_raw.df$volatile.acidity
citric.acid = train_raw.df$citric.acid
residual.sugar = train_raw.df$residual.sugar
chlorides = train_raw.df$chlorides
free.sulfur.dioxide = train_raw.df$free.sulfur.dioxide
total.sulfur.dioxide = train_raw.df$total.sulfur.dioxide
density = train_raw.df$density
pH = train_raw.df$pH
sulphates = train_raw.df$sulphates
alcohol = train_raw.df$alcohol
quality = train_raw.df$quality




# use the step function for model selection 
# feature selection by exhaustive selection and backward selection
regfit.full <- regsubsets(quality ~  fixed.acidity  +  volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol ,data = train_raw.df )
regsum <- summary(regfit.full)

regfit.fwd<-regsubsets(quality ~  fixed.acidity  +  volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol,data = train_raw.df, method='forward')
regsum <- summary(regfit.fwd)

names(regsum)

which.min(regsum$bic)

regsum$bic

plot(regfit.full,scale='bic')
plot(regfit.fwd,scale='bic')


# build linear model

linear_model <- lm(quality ~ volatile.acidity + citric.acid + sulphates + alcohol + density + total.sulfur.dioxide ,data = train_raw.df ) # by correlation

linear_model <- lm(quality  ~ volatile.acidity + chlorides + total.sulfur.dioxide + pH + sulphates + alcohol,data = train_raw.df )

summary(linear_model)

table(round(predict(linear_model,test_raw.df[,1:11])),test_raw.df[,12])


( 1 + 125  + 115 + 11 )/nrow(test_raw.df)

(125 + 119 + 10)/nrow(test_raw.df)



# use 10-fold cross validation 

train.control <- trainControl(method = "cv", number = 10) # only for one model 
model <- train(quality ~., data = data, method = "lm",
               trControl = train.control)
print(model) 

# use 10-fold cross validation for feature selection

train<-sample(c(T,F),nrow(data),rep=T)
sum(train)
test<-!train


k<-30
folds<-sample(1:k,nrow(data),replace=T)


cv.errors<-matrix(NA,k,8,dimnames=list(NULL,paste(1:8)))
regfit.best <-regsubsets(quality~.,data=data[train,])

summary(regfit.best)


for(i in 1:k){
  best.fit<-regsubsets(quality ~ . , data=data[folds!=i,])
  test.mat<-model.matrix(quality ~ . ,data=data[folds==i,])
  for(j in 1:8){
    coefi<-coef(regfit.best,j)
    pred<-test.mat[,names(coefi)]%*%coefi
    cv.errors[i,j]<-mean((data$quality[folds==i]-pred)^2)
  }
}


mean.cv.errors<-apply(cv.errors,2,mean)
which.min(mean.cv.errors)

reg.best<-regsubsets(quality~.,data=data)

coef(reg.best,8)

summary(reg.best)


# predict(coef(reg.best,8), test[,1:11])

# use Naive Bayes Classifier

classifier_cl <- naiveBayes( quality ~ volatile.acidity + citric.acid + sulphates + alcohol + density + total.sulfur.dioxide ,data = train_raw.df )
classifier_cl

predict(classifier_cl, newdata = test_raw.df)

table(predict(classifier_cl, newdata = test_raw.df),test_raw.df[,12])

#the accuracy rate :
sum(diag(table(predict(classifier_cl, newdata = test_raw.df),test_raw.df[,12])))/nrow(test_raw2.df)

# use LDA

train_raw.lda <- lda(quality ~  fixed.acidity  +  volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol, data = train_raw.df)

test_raw.lda.predict <- predict(train_raw.lda, newdata = test_raw.df)

match=test_raw.df[,12]- (as.numeric(test_raw.lda.predict$class) +2)

sum(match==0)/length(match)

