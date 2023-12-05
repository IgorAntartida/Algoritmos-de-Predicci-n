choose.files()
insurance<-read.csv("C:\\Users\\HP\\Downloads\\insurance.csv")  

library(dplyr)

insurance$gender<- factor(insurance$gender, levels = c(1,0), labels = c("Si","No") )


#Muestreo 

library(tidyverse)
library(tidymodels)
library(rpart.plot)

library(rpart)

set.seed(1234)


insurance_split <- initial_split(insurance, prop = 0.80, strata="claim")

insurance_train <- training(insurance_split)

insurance_test <- testing(insurance_split)

folds <- vfold_cv(insurance_train, v = 10, strata="claim")

metric <- metric_set(accuracy,recall, precision)

#Modelo de arbol de regresion 
tree_spec <-
  decision_tree(
    mode = "classification", 
    tree_depth = tune("depth"),
    engine = "rpart"
  ) 

tree_rec <-
  recipe(gender ~ ., data = insurance_train) 

tree_wflow <- 
  workflow() %>% 
  add_model(tree_spec) %>%
  add_recipe(tree_rec)

tree_grid <- expand.grid(depth=c(3,5,10,20))

tree_res <- 
  tune_grid(
    tree_wflow,
    resamples = folds,
    metrics = metric,
    grid = tree_grid
  )

collect_metrics(tree_res)
autoplot(tree_res)

show_best(tree_res,metric="accuracy")

# Best Tree

best_tree_spec <-
  decision_tree(
    mode = "classification", 
    tree_depth = 5,
    engine = "rpart"
  ) 

best_tree_wflow <- 
  workflow() %>% 
  add_model(best_tree_spec) %>%
  add_recipe(tree_rec)

best_tree_fit <- last_fit(best_tree_wflow, insurance_split)

collect_metrics(best_tree_fit)

# Predicting new observations

best_tree_model <- fit(best_tree_wflow,data=insurance_train)
clasification_tree <- predict(best_tree_model,new_data=insurance_test)

# Graph best tree 

# Extract information from engine 'rpart'

a=extract_fit_engine(best_tree_model)

library(rpart.plot)

rpart.plot(a,box.palette="RdBu",shadow.col="grey",nn=TRUE,roundint = FALSE)
