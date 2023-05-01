install.packages("caret", dependencies=c("Depends", "Suggests"))
install.packages("ModelMetrics")
install.packages("ggcorrplot")
library(caret)
library(tidyverse)
library(ggcorrplot)
library(mongolite)

data(iris)

########
## MONGOLITE DATA COLLECTION
########

connection_string = 'mongodb+srv://Lann0007:zw7Bi5ikPUbmwrqz@comp2031.x9dfsly.mongodb.net/?retryWrites=true&w=majority'
customer_entries = mongo(collection= "customers", db = "sample_analytics", url = connection_string)
transaction_entries = mongo(collection= "customers", db = "sample_analytics", url = connection_string)

account_types = customer_entries$aggregate('[{"$project":{"name": 1 ,"address": 1 }}]')
user_types = customer_entries$aggregate('[{"$group":{"_id":"$usertype", "Count": {"$sum":1}}}]')

df <- as_tibble(account_types)

addresses <- df %>% pull(address)
addresses <- as.data.frame(addresses)
addresses[c("first section","second section")] <- str_split_fixed(addresses$address, ', ',2)
addresses <- addresses[3]


vec <- c()
vec
for (i in 1:nrow(addresses)){
  if(addresses[i, 1] == "" || nchar(addresses[i, 1]) > 9 ){
    vec <- append(vec, i)
  }
}

addresses[487, 1] == ""
vec
addresses[c("second section")]
cleaned_addresses <-addresses[-vec,]
cleaned_addresses

df %>% separate_wider_delim(address, ",", names = c("A", "B","Something"))

dataset <- iris
dataset

########
## NORMALISATION
########

numerical_data <- dataset[, 4]

head(numerical_data)

data_normalized <- scale(numerical_data)
head(data_normalized)

corr_matrix <- cor(data_normalized)

ggcorrplot(corr_matrix)

data.pca <- princomp(corr_matrix)
summary(data.pca)

data.pca$loadings

########
## TRAINING AND VALIDATION ALLOCATION
########


validation_index <- createDataPartition(dataset$Species, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- dataset[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset <- dataset[validation_index,]
# dimension of the dataset
dim(dataset)

sapply(dataset, class)

head(dataset)

levels(dataset$Species)

percentage <- prop.table(table(dataset$Species)) * 100
cbind(freq=table(dataset$Species), percentage=percentage)

########
## GRAPHS
########

summary(dataset)

x <- dataset[,1:4]
y <- dataset[,5]

par(mfrow=c(1,4))
    for(i in 1:4){
        boxplot(x[,i], main=names(iris)[i])
    }

plot(y)

# scatterplot matrix
featurePlot(x=x, y=y, plot="ellipse")

# box and whisker plots for each attribute
featurePlot(x=x, y=y, plot="box")

control <- trainControl(method="cv", number=10)
metric <- "Accuracy"


########
## ACTUAL TRAINING
########

# a) linear algorithms
set.seed(7)
fit.lda <- train(Species~., data=dataset, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(Species~., data=dataset, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(Species~., data=dataset, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(Species~., data=dataset, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(Species~., data=dataset, method="rf", metric=metric, trControl=control)

# summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

print(fit.knn)

predictions <- predict(fit.knn, validation)
confusionMatrix(predictions, validation$Species)
