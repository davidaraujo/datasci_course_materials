library(caret)
library(ggplot2)
library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)
library(randomForest)
library(e1071)
seaflow <- read.csv("seaflow_21min.csv")

seaflow <- na.omit(seaflow)
#str(seaflow)
#sample_data1 <- createDataPartition(seaflow, times = 1,p = 0.5,list = TRUE,groups = min(5, length(y)))
#sample_data2 <- sample(seaflow, trunc(length(seaflow)/2))

## 50% of the sample size
smp_size <- floor(0.5 * nrow(seaflow))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(seaflow)), size = smp_size)

train <- seaflow[train_ind, ]
test <- seaflow[-train_ind, ]

# show in a plot
#ggplot(aes(x=chl_small,y=pe,color=pop),data=seaflow)+geom_jitter()

fol <- formula(pop ~ fsc_small + fsc_perp + fsc_big + pe + chl_big + chl_small)

## fit a decision tree model
model_dt <- rpart(fol, method="class", data=train)
#print(model)
# show the decision tree
#rpart.plot(model,branch=0,branch.type=2,type=1,extra=102,shadow.col="pink",box.col="green",split.col="red")

test <- na.omit(test)

#predict the outcome of the testing data
pred_dt <- predict(model_dt, test)
#accuracy
#1-sum(diag(pred_dt))/sum(pred_dt)

## fit a random tree model
model_rf <- randomForest(fol, data=train)
#print(model_rf)
pred_rf <- predict(model_rf, test)
# get the gini impurity measure
importance(model_rf)
# plot random forest
#plot(model_rf)

# table

## fit a support vector machine model
model_sv <- svm(fol, data=train)
pred_sv <- predict(model_sv, test)
print(model_sv)


# tables
table(pred = pred_dt, true =test$pop)
table(pred = pred_rf, true =test$pop)
table(pred = pred_sv, true =test$pop)

qplot(fsc_big,data=seaflow)
