#Project: Fraud Detection
#Author: Pritesh Contractor
#Created on July 30, 2020

# install all libraries which are not present and required

if(!require(tidyverse)) install.packages("tidyverse") 
if(!require(kableExtra)) install.packages("kableExtra")
if(!require(tidyr)) install.packages("tidyr")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(stringr)) install.packages("stringr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(gbm)) install.packages("gbm")
if(!require(dplyr)) install.packages("dplyr")
if(!require(caret)) install.packages("caret")
if(!require(xgboost)) install.packages("xgboost")
if(!require(e1071)) install.packages("e1071")
if(!require(class)) install.packages("class")
if(!require(ROCR)) install.packages("ROCR")
if(!require(randomForest)) install.packages("randomForest")
if(!require(PRROC)) install.packages("PRROC")
if(!require(reshape2)) install.packages("reshape2")
if(!require(googledrive)) install.packages("googledrive")
if(!require(purrr)) install.packages("purrr")

# loading required libraries

library(dplyr)
library(tidyverse)
library(kableExtra)
library(tidyr)
library(ggplot2)
library(gbm)
library(caret)
library(xgboost)
library(e1071)
library(class)
library(ROCR)
library(randomForest)
library(PRROC)
library(reshape2)
library(googledrive)
library(purrr)

# restricting google drive to not attempt for getting or sending token
#drive_auth_config(active = FALSE)

# downloading csv file from google drive and saving into temp folder

# the link is made readable to world

folder_url <- "https://drive.google.com/file/d/1DnIYCJBlqOpatH-juUgJDN2JXVYQZwb9/view?usp=sharing"
folder <- drive_get(as_id(folder_url))
#if (dir.exists("C:\\Users\\nisha\\AppData\\Local\\Temp\\Rtmpq8dIel\\")){
  downloadpath <- tempfile(fileext="creditcard.csv")
  drive_download(as_id(folder_url),path=downloadpath, overwrite = TRUE)#} else {#
  #dir.create("C:\\Users\\nisha\\AppData\\Local\\Temp\\Rtmpq8dIel\\")
  #downloadpath <- tempfile(fileext="creditcard.csv")
  #drive_download(as_id(folder_url),path=downloadpath, overwrite = TRUE)
#}

# closing all unwanted connections
# on.exit(closeAllConnections())

# loading dataset
readcsv <- read.csv(downloadpath, header = TRUE, sep=",")

# Get length and columns details from the dataset

data.frame("Length"=nrow(readcsv), "Columns"=ncol(readcsv)) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped","hover","condensed","responsive"),
                position="center",
                font_size = 12,
                full_width = FALSE)

dim <- data.frame(readcsv)
dim$class = ifelse(readcsv$Class==0,'Legal Transaction','Fraud Transaction') %>% as.factor()

# Visualize class proportion

dim %>%
  ggplot(aes(Class)) + 
  theme_minimal() +
  geom_bar() +
  scale_x_discrete() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Proportions between Legal and Frauds Transactions",
       x = "Class",
       y = "Frequency")

# identifying missing values 

sapply(readcsv, function(x) sum(is.na(x))) %>% 
  kable(col.names = c("Missing Values")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)

# Histogram - frauds transaction amounts distribution  

readcsv[readcsv$Class == 1,] %>%
  ggplot(aes(Amount)) + 
  theme_minimal()  +
  geom_histogram(binwidth = 40) +
  labs(title = "Fraud Amounts Distributions - Histogram",
       x = "Dollars Amount",
       y = "Frequency")

# Extracting counts per amount

readcsv[readcsv$Class == 1,] %>%
  group_by(Amount) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(n=10) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)

# Histogram - Fraud transaction over time distribution 

readcsv[readcsv$Class == 1,] %>%
  ggplot(aes(Time)) + 
  theme_minimal()  +
  geom_histogram(binwidth = 40) +
  labs(title = "Fraud over Time Distributions - Histogram",
       x = "Time",
       y = "Frequency")

# identifying count per time

readcsv[readcsv$Class == 1,] %>%
  group_by(Time) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(n=10) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)

# identifying correlation matrix - lower traig

get_lower_triag <- function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

# identifying correlation matrix - upper traig

get_upper_triag <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

# creating function reorder cormat - using correlation between variables as distance 

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

corr_matrix <- round(cor(readcsv),2)
corr_matrix <- reorder_cormat(corr_matrix)

upper_traig <- get_upper_triag(corr_matrix)

melted_corr_matrix <- melt(upper_traig, na.rm = TRUE)

# ggplot for pearson correlation of the dataset

ggplot(melted_corr_matrix, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 9, hjust = 1), axis.text.y = element_text(size = 9),axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank()) +
  coord_fixed()

# removing the time column from the dataset 

readcsv$Class <- as.factor(readcsv$Class)
readcsv <- readcsv %>% select(-Time)

# split the dataset into train, test and cross validation dataset 

train_index <- createDataPartition(
  y = readcsv$Class, 
  p = .6, 
  list = F
)

train <- readcsv[train_index,]

test_cv <- readcsv[-train_index,]

test_index <- createDataPartition(
  y = test_cv$Class, 
  p = .5, 
  list = F)

test <- test_cv[test_index,]
cv <- test_cv[-test_index,]

rm(train_index, test_index, test_cv)

# Baseline model creation which predicts Legal Transaction only with 0 and compute all metrics 

# cloning the data frame 

baseline_model <- data.frame(readcsv)

# Setting up the class al to Legal (0)

baseline_model$Class = factor(0, c(0,1))

# Predicting using baseline model 

baseline_pred <- prediction(
  as.numeric(as.character(baseline_model$Class)),as.numeric(as.character(readcsv$Class))
)

# Computing AUC and AUCPR 

auc_val_baseline <- performance(baseline_pred, "auc")
auc_plot_baseline <- performance(baseline_pred, 'sens', 'spec')
aucpr_plot_baseline <- performance(baseline_pred, "prec", "rec")

# creating relative plot 

plot(auc_plot_baseline, 
     main=paste("Baseline Model - AUC:", 
                auc_val_baseline@y.values[[1]])
)

plot(aucpr_plot_baseline, main="Baseline Model - AUCPR: 0")

# Create Data Frame Outcome to enter the results obtained through Based Models and will be using same going forward for adding the metrics to the same
# Create data frame 'outcome' that contains all metrics obtained from the Base models 

outcome <- data.frame(
  Model = "Baseline - Predict Always Legal", 
  AUC = auc_val_baseline@y.values[[1]],
  AUCPR = 0
)

# display outcome in a table 

outcome %>%
  kable() %>%
  kable_styling(
    bootstrap_options = 
      c("striped", "hover", "condensed", "responsive"),
    position = "center",
    font_size = 10,
    full_width = FALSE
  ) 

# Naive Bayes Model creation which will improve the outcome in AUC and AUCPR 

# Building Naive Bayes Model  with Class as a target and all other variables as predictor 


naive_model <- naiveBayes(Class ~ ., data = train, laplace=1)

# Prediction outcome 

naive_model_predictions <- predict(naive_model, newdata=test)

# Computing AUC and AUCPR for the Naive Model 

naive_pred <- prediction(as.numeric(naive_model_predictions) , test$Class)

auc_val_naive <- performance(naive_pred, "auc")

auc_plot_naive <- performance(naive_pred, 'sens', 'spec')
aucpr_plot_naive <- performance(naive_pred, "prec", "rec")

aucpr_val_naive <- pr.curve(
  scores.class0 = naive_model_predictions[test$Class == 1], 
  scores.class1 = naive_model_predictions[test$Class == 0],
  curve = T,  
  dg.compute = T
)

# Creating the relative plots 

plot(aucpr_val_naive)
plot(auc_plot_naive, main=paste("Naive Bayes Classifier - AUC:", auc_val_naive@y.values[[1]]))
plot(aucpr_plot_naive, main=paste("Naive Bayes Classifier - AUCPR:", aucpr_val_naive$auc.integral))

# Adding the respective metrics to the results dataset

outcome <- outcome %>% add_row(
  Model = "Naive Bayes", 
  AUC = auc_val_naive@y.values[[1]],
  AUCPR = aucpr_val_naive$auc.integral
)

# Display outcome in a table 

outcome %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed","responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE) 

# KNN Model with class as target and all other variables as predictors where k is set to 5 

knn_model <- knn(train[,-30], test[,-30], train$Class, k=5, prob = TRUE)

# computing AUC and AUCPR for KNN Model 

knn_pred <- prediction(
  as.numeric(as.character(knn_model)),as.numeric(as.character(test$Class))
)

auc_val_knn <- performance(knn_pred, "auc")

auc_plot_knn <- performance(knn_pred, 'sens', 'spec')
aucpr_plot_knn <- performance(knn_pred, "prec", "rec")

aucpr_val_knn <- pr.curve(
  scores.class0 = knn_model[test$Class == 1], 
  scores.class1 = knn_model[test$Class == 0],
  curve = T,  
  dg.compute = T
)

# Creating relative plot 

plot(aucpr_val_knn)
plot(auc_plot_knn, main=paste("K Nearest Neighbour - AUC:", auc_val_knn@y.values[[1]]))
plot(aucpr_plot_knn, main=paste("K Nearest Neighbour - AUCPR:", aucpr_val_knn$auc.integral))


# Adding metrics to the outcome dataset 

outcome <- outcome %>% add_row(
  Model = "K-Nearest Neighbors k=5", 
  AUC = auc_val_knn@y.values[[1]],
  AUCPR = aucpr_val_knn$auc.integral
)

# display outcome in table 

outcome %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed","responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE) 

# Predicting through SVM Model - Support Vector Model with class as target and all other variables as predictors. Please note the kernet is set to sigmoid 

svm_model <- svm(Class ~ ., data = train, kernel='sigmoid')

# Predicting bassed on this model 

svm_model_predictions <- predict(svm_model, newdata=test)

# computing AUC and AUCPR through SVM Model predictions 

svm_pred <- prediction(
  as.numeric(as.character(svm_model_predictions)), as.numeric(as.character(test$Class))
)

auc_val_svm <- performance(svm_pred, "auc")

auc_plot_svm <- performance(svm_pred, 'sens', 'spec')
aucpr_plot_svm <- performance(svm_pred, "prec", "rec")

aucpr_val_svm <- pr.curve(
  scores.class0 = svm_model_predictions[test$Class == 1], 
  scores.class1 = svm_model_predictions[test$Class == 0],
  curve = T,  
  dg.compute = T
)

# Creating relative path for SVM Model 

plot(aucpr_val_svm)
plot(auc_plot_svm, main=paste("Support Vector Machine - SVM - AUC:", auc_val_svm@y.values[[1]]))
plot(aucpr_plot_svm, main=paste("Support Vector Machine - SVM - AUCPR:", aucpr_val_svm$auc.integral))

# Adding the metrics to the outcome dataset

outcome <- outcome %>% add_row(
  Model = "SVM - Support Vector Machine",
  AUC = auc_val_svm@y.values[[1]],
  AUCPR = aucpr_val_svm$auc.integral)

# display output in a table 

outcome %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed",           "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)

#Random forest Model creation with class as target and all other variables as predictors. For the model we have set number of trees to 500

random_forest_model <- randomForest(Class ~ ., data = train, ntree = 500)

# extracting the feature importance 

feature_imp <- data.frame(importance(random_forest_model))

# Making prediction based on the model 

random_predictions <- predict(random_forest_model, newdata=test)

# computing the AUC and AUCPR 

random_pred <- prediction(
  as.numeric(as.character(random_predictions)),as.numeric(as.character(test$Class))
)

auc_val_rf <- performance(random_pred, "auc")

auc_plot_rf <- performance(random_pred, 'sens', 'spec')

aucpr_plot_rf <- performance(random_pred, "prec", "rec", curve = T,  dg.compute = T)

aucpr_val_rf <- pr.curve(scores.class0 = random_predictions[test$Class == 1], scores.class1 = random_predictions[test$Class == 0],curve = T,  dg.compute = T)

# creating relative path for predictions generated through random forest model 

plot(auc_plot_rf, main=paste("Random Forest - AUC:", auc_val_rf@y.values[[1]]))
plot(aucpr_plot_rf, main=paste("Random Forest - AUCPR:", aucpr_val_rf$auc.integral))
plot(aucpr_val_rf)

# adding respective metrics generate to outcome data set 

outcome <- outcome %>% add_row(
  Model = "Random Forest",
  AUC = auc_val_rf@y.values[[1]],
  AUCPR = aucpr_val_rf$auc.integral)

# display output in a table 

outcome %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed",           "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)

# display feature importance in a table 

feature_imp %>% kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed",           "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)

# GBM Modle - Generalized Boosted Model - building with class as traget and all other variables as predictors
# distribution used is bernoully and number of tree set is 500 

gbm_model <- gbm(as.character(Class) ~ .,
                 distribution = "bernoulli", 
                 data = rbind(train, test), 
                 n.trees = 500,
                 interaction.depth = 3, 
                 n.minobsinnode = 100, 
                 shrinkage = 0.01, 
                 train.fraction = 0.7,
)

# identifying the best iteration using the test data 

best_iteration <- gbm.perf(gbm_model, method = "test")

# Calculating predictions based on the GBM Model 

gbm_predictions = predict.gbm(
  gbm_model, 
  newdata = test, 
  n.trees = best_iteration, 
  type="response"
)

# calculating feature importance 

feature_imp_gbm_model = summary(gbm_model, n.trees = best_iteration)

# Calculating AUC and AUCPR through the prediction generated from GBM Model 

gbm_pred <- prediction(
  as.numeric(as.character(gbm_predictions)), as.numeric(as.character(test$Class))
)

auc_val_gbm <- performance(gbm_pred, "auc")

auc_plot_gbm <- performance(gbm_pred, 'sens', 'spec')
aucpr_plot_gbm <- performance(gbm_pred, "prec", "rec")

aucpr_val_gbm <- pr.curve(
  scores.class0 = gbm_predictions[test$Class == 1], 
  scores.class1 = gbm_predictions[test$Class == 0],
  curve = T,  
  dg.compute = T
)

# generating relative path plot 

plot(aucpr_val_gbm)
plot(auc_plot_gbm, main=paste("GBM - Model - AUC:", auc_val_gbm@y.values[[1]]))
plot(aucpr_plot_gbm, main=paste("GBM - Model - AUCPR:", aucpr_val_gbm$auc.integral))

# Adding respective metrics to the outcome datasets 

outcome <- outcome %>% add_row(
  Model = "GBM - Generalized Boosted Regression",
  AUC = auc_val_gbm@y.values[[1]],
  AUCPR = aucpr_val_gbm$auc.integral)

# display outcome in a table 

outcome %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed",           "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)

# dispaly the feature importance in a table 

feature_imp_gbm_model %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed",           "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE) 

# Building / Preparing the training dataset for XGB Model 


xgb_model_train <- xgb.DMatrix(
  as.matrix(train[, colnames(train) != "Class"]), 
  label = as.numeric(as.character(train$Class))
)

# building / Preparing the test data set for XGB Model - XG Boost Model 

xgb_model_test <- xgb.DMatrix(
  as.matrix(test[, colnames(test) != "Class"]), 
  label = as.numeric(as.character(test$Class))
)

# building and preparing the cv data set for XGB Model

xgb_model_cv <- xgb.DMatrix(
  as.matrix(cv[, colnames(cv) != "Class"]), 
  label = as.numeric(as.character(cv$Class))
)

# Parameter list preparation 

xgb_model_params<- list(
  objective = "binary:logistic", 
  eta = 0.1, 
  max.depth = 3, 
  nthread = 6, 
  eval_metric = "aucpr"
)

# Train the XGBoost Model 

xgb_model <- xgb.train(
  data = xgb_model_train, 
  params = xgb_model_params, 
  watchlist = list(test = xgb_model_test, cv = xgb_model_cv), 
  nrounds = 500, 
  early_stopping_rounds = 40, 
  print_every_n = 20
)

# Calculating feature importance 

feature_xgb_imp <- xgb.importance(colnames(train), model = xgb_model)

# plot relative importance 

xgb.plot.importance(feature_xgb_imp, rel_to_first = TRUE, xlab = "Relative importance")

# Calculate predictions based on the XGB Model 

xgb_predictions = predict(
  xgb_model, 
  newdata = as.matrix(test[, colnames(test) != "Class"]), 
  ntreelimit = xgb_model$bestInd
)

# calculating the AUC and AUCPR 

xgb_pred <- prediction(
  as.numeric(as.character(xgb_predictions)),as.numeric(as.character(test$Class))
)

auc_val_xgb <- performance(xgb_pred, "auc")

auc_plot_xgb <- performance(xgb_pred, 'sens', 'spec')
aucpr_plot_xgb <- performance(xgb_pred, "prec", "rec")

aucpr_val_xgb <- pr.curve(
  scores.class0 = xgb_predictions[test$Class == 1], 
  scores.class1 = xgb_predictions[test$Class == 0],
  curve = T,  
  dg.compute = T
)

# Relative Plot

plot(auc_plot_xgb, main=paste("XGBoost - AUC:", auc_val_xgb@y.values[[1]]))
plot(aucpr_plot_xgb, main=paste("XGBoost - AUCPR:", aucpr_val_xgb$auc.integral))
plot(aucpr_val_xgb)

# adding the respective metrics to the outcome datasets 

outcome <- outcome %>% add_row(
  Model = "XGBoost",
  AUC = auc_val_xgb@y.values[[1]],
  AUCPR = aucpr_val_xgb$auc.integral)

# display outcome in a table 

outcome %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed",           "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)

# display feature importance in a table 

feature_xgb_imp %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed",           "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)


