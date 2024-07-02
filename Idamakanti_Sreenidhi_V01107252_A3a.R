satisfaction <- read.csv("D:\\CHRIST\\Boot camp\\DATA\\Airline_customer_satisfaction.csv")

library(dplyr)
library(ggplot2)
library(DataExplorer)

satisfaction
plot_missing(satisfaction)
summary(satisfaction)
sum(is.na(satisfaction))
names(satisfaction)
str(satisfaction)

# Replace missing values with the mean for numeric columns
satisfaction <- satisfaction %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
#Checking missing values after filling it with mean values of the column 
missing_info <- colSums(is.na(satisfaction))
cat("Missing Values Information:\n")
print(missing_info)

#performing logistic regression , validate assumptions , and evaluating the performance with a confusion matrix
#and ROC curve and interpreting the results 
names(satisfaction)


cor_matrix <- cor(Filter(is.numeric, satisfaction))
print(cor_matrix)
heatmap(cor_matrix)
heatmap(cor_matrix)
boxplot(Age ~ satisfaction + Type.of.Travel, data=satisfaction, main="Boxplot of Age by Satisfaction and Type of Travel", xlab="Satisfaction and Type of Travel", ylab="Age")






library(dplyr)
library(caTools)
library(pROC)
library(rpart)
library(rpart.plot)
library(MLmetrics)

# Logistic Regression
set.seed(123)
split<-sample.split(satisfaction$satisfaction,SplitRatio = 0.7)
train<-subset(satisfaction,split==TRUE)
test<-subset(satisfaction,split==FALSE)
unique(train$satisfaction)

# Let's say the unique values are 'Satisfied' and 'Not Satisfied'
# We can convert them to 0 and 1 using the ifelse function
train$satisfaction <- ifelse(train$satisfaction == 'Satisfied', 1, 0)

# Now you can run your model:
model <- glm(satisfaction ~ ., data = train, family = binomial)
pred_prob<-predict(model,newdata=test,type="response")
pred_class<-ifelse(pred_prob >= 0.5,1,0)
# Confusion Matrix 
library(MLmetrics)
# Remove rows with missing predictions
test_complete <- test[complete.cases(pred_class), ]

# Calculate the confusion matrix
confusion <- ConfusionMatrix(factor(pred_class), factor(test_complete$satisfaction))
print(confusion)
roc_obj<-roc(test$satisfaction,pred_prob)
auc<-auc(roc_obj)
print(paste("AUC-ROC:",auc))
plot(roc_obj,main="ROC Curve",print.auc=TRUE)

#decision tree analysis for the data in part A and compare the results of the 
#Logistic regression and Decision tree
library(stats)
#install.packages("rpart")
library(rpart)
#install.packages("caTools")
library(caTools)



set.seed(123)
split<-sample.split(satisfaction$satisfaction,SplitRatio = 0.7)
train<-subset(satisfaction,split == TRUE)
test<-subset(satisfaction,split == FALSE)
model<-rpart(satisfaction~.,data=train,method="class")
pred_prob<-predict(model,newdata=test,type="prob")
pred_class<-ifelse(pred_prob[,2]>=0.5,1,0)

confusion <- ConfusionMatrix(factor(pred_class),factor(test$satisfaction))
print(confusion)
roc_obj<-roc(test$satisfaction,pred_prob[,2])
auc<-auc(roc_obj)
print(paste("AUC-ROC:",auc))
plot(roc_obj,main="ROC Curve",print.auc=TRUE)