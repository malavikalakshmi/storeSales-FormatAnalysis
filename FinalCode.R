# Load Libraries
library(caret)
library(corrplot)
library(mlbench)
# 1. Load Data
library(readxl)

dataset <- read_excel("storedemographicdata85.xlsx")

# calculate the pre-process parameters from the dataset
preprocessParams <- preProcess(dataset, method=c("center", "scale", "pca"))
# summarize transform parameters
print(preprocessParams)
# transform the dataset using the parameters
transformed <- predict(preprocessParams, dataset)
# summarize the transformed dataset
summary(transformed)

# correlation plot
correlations <- cor(transformed[,2:18])
corrplot(correlations, method="circle")


transformed2 <- transformed[1:85,]
transformed3 <- transformed[86:95,]
dataset <- transformed2
# 2. Split out validation dataset
# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(dataset$Cluster, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- dataset[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset <- dataset[validation_index,]


# 5. Evaluate Algorithms
# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"
# a) linear algorithms
set.seed(7)
fit.lda <- train(Cluster~., data=dataset, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(Cluster~., data=dataset, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(Cluster~., data=dataset, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(Cluster~., data=dataset, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(Cluster~., data=dataset, method="rf", metric=metric, trControl=control)

# d) compare algorithms
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)
dotplot(results)
# summarize Best Model
print(fit.rf)

# estimate skill on validation dataset
set.seed(9)

predictions <- predict(fit.svm, newdata=validation)
print(predictions)
confusionMatrix(as.factor(predictions), as.factor(validation$Cluster))


predictions <- predict(fit.rf, newdata=transformed3)
print(predictions)


# collect resamples
results <- resamples(list(CART=fit.cart, LDA=fit.lda, SVM=fit.svm, KNN=fit.knn, RF=fit.rf))
# box and whisker plots to compare models
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results, scales=scales)

diffs <- diff(results)
# summarize p-values for pair-wise comparisons
summary(diffs)
# plot of differences
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(diffs, scales=scales)
# t-test between two models
compare_models(fit.svm, fit.lda)






