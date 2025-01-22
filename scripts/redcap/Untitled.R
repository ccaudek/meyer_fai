
######################################

```{r}
# Calcola i pesi
# Calcolo delle frequenze delle classi nel dataset
class_weights <- table(fai$nome_malattia_raggruppato)

# Calcolo dei pesi inversi per ciascuna osservazione
weights <- 1 / class_weights[fai$nome_malattia_raggruppato]
weights <- as.numeric(weights)

# Modello multinomiale con pesi
fm <- multinom(
  nome_malattia_raggruppato ~ .,
  data = fai,
  weights = weights
)
```

```{r}
predictions <- predict(fm, newdata = fai)
table(Predicted = predictions, Actual = fai$nome_malattia_raggruppato)

accuracy <- mean(predictions == fai$nome_malattia_raggruppato)
print(paste("Accuracy:", round(accuracy, 2)))
# [1] "Accuracy: 1"
```

```{r}
# Dividi i dati in training e test
set.seed(12345)
train_index <- createDataPartition(
  fai$nome_malattia_raggruppato, 
  p = 0.7, 
  list = FALSE
)
train <- fai[train_index, ]
test <- fai[-train_index, ]

# Calcolo delle frequenze delle classi nel set di training
class_weights <- table(train$nome_malattia_raggruppato)

# Calcolo dei pesi per il set di training
weights <- 1 / class_weights[train$nome_malattia_raggruppato]
weights <- as.numeric(weights)

# Modello multinomiale con pesi
fm <- multinom(
  nome_malattia_raggruppato ~ .,
  data = train,
  weights = weights
)
```

```{r}
table(test$nome_malattia_raggruppato)
```

```{r}
train_predictions <- predict(fm, newdata = train)
train_accuracy <- mean(train_predictions == train$nome_malattia_raggruppato)
print(paste("Training Accuracy:", round(train_accuracy, 2)))
```

## Cross validation

### Motivation

This analysis aims to evaluate the performance of a penalized multinomial regression model in classifying four disease severity categories: *Lieve* (Mild), *Grave* (Severe), *Moderata* (Moderate), and *Molto grave* (Very Severe). The model is trained using cross-validation to identify the optimal regularization parameter (`decay`), which penalizes overfitting by shrinking coefficient estimates. The optimal parameter is then applied to a test set to assess the generalizability of the model's performance. The evaluation metrics include accuracy, Kappa statistic, and class-specific measures such as sensitivity, specificity, positive predictive value (PPV), and balanced accuracy. 

The primary objective is to determine whether the model can effectively classify observations into their respective severity categories while accounting for the imbalanced class distribution.

```{r}
#| output: false
#| 
# Configura la cross-validation
ctrl <- trainControl(method = "cv", number = 50)

# Esegui il modello con cross-validation
cv_model <- train(
  nome_malattia_raggruppato ~ .,
  data = train,
  method = "multinom",
  trControl = ctrl,
  weights = weights
)

# Risultati della cross-validation
# print(cv_model)
```

```{r}
# Predizioni con il modello ottimale
test_predictions <- predict(cv_model, newdata = test)

# Matrice di confusione
confusionMatrix(
  data = test_predictions,
  reference = test$nome_malattia_raggruppato
)
```

## Oversampling and Penalized Multinomial Regression

```{r}
# Step 1: Split data into training and test sets
set.seed(12345)
train_index <- createDataPartition(
  fai$nome_malattia_raggruppato, p = 0.7, list = FALSE)
train <- fai[train_index, ]
test <- fai[-train_index, ]

# Step 2: Balance the training set using upsampling
recipe_obj <- recipe(nome_malattia_raggruppato ~ ., data = train) %>%
  step_upsample(nome_malattia_raggruppato, over_ratio = 1, seed = 123)

# Prepare the balanced training data
balanced_train <- prep(recipe_obj) %>% juice()

# Step 3: Fit a penalized multinomial regression model
class_weights <- table(balanced_train$nome_malattia_raggruppato)
weights <- 1 / class_weights[balanced_train$nome_malattia_raggruppato]
weights <- as.numeric(weights)

fm <- multinom(
  nome_malattia_raggruppato ~ .,
  data = balanced_train,
  weights = weights,
  decay = 1e-04  # Optimal decay value found in cross-validation
)

# Step 4: Evaluate the model on the test set
test_predictions <- predict(fm, newdata = test)

# Confusion matrix and performance metrics
conf_matrix <- confusionMatrix(
  data = test_predictions,
  reference = test$nome_malattia_raggruppato
)

# Print results
print(conf_matrix)
```

```{r}
# Create a recipe with upsampling
recipe_obj <- recipe(nome_malattia_raggruppato ~ ., data = train) %>%
  step_upsample(nome_malattia_raggruppato, over_ratio = 1, seed = 123)

# Prepare the balanced data
balanced_train <- prep(recipe_obj) %>% juice()

# Check the new class distribution
table(balanced_train$nome_malattia_raggruppato)
```

```{r}
# Fit the multinomial regression model
fm_balanced <- multinom(
  nome_malattia_raggruppato ~ ., 
  data = balanced_train,
  decay = 1e-04  # Regularization parameter
)

# Predict on the test set
test_predictions_balanced <- predict(fm_balanced, newdata = test)

# Evaluate performance
library(caret)
conf_matrix_balanced <- confusionMatrix(
  data = test_predictions_balanced,
  reference = test$nome_malattia_raggruppato
)
print(conf_matrix_balanced)
```



## Random Forest

```{r}
# # Fit a Random Forest model
# rf_model <- randomForest(
#   nome_malattia_raggruppato ~ ., 
#   data = train,
#   ntree = 500,  # Number of trees
#   classwt = class_weights,  # Apply class weights
#   importance = TRUE
# )
# 
# # Predict on the test set
# test_predictions_rf <- predict(rf_model, newdata = test)
# 
# # Evaluate performance
# conf_matrix_rf <- confusionMatrix(
#   data = test_predictions_rf,
#   reference = test$nome_malattia_raggruppato
# )
# print(conf_matrix_rf)
# 
# # Variable importance
# varImpPlot(rf_model)
```

```{r}
# Calculate class weights inversely proportional to class frequencies
class_weights <- 1 / table(train$nome_malattia_raggruppato)

# Fit Random Forest with class weights
rf_weighted <- randomForest(
  nome_malattia_raggruppato ~ ., 
  data = train,
  ntree = 500, 
  importance = TRUE, 
  classwt = class_weights  # Apply class weights
)

# Predict and evaluate
test_predictions_rf_weighted <- predict(rf_weighted, newdata = test)
conf_matrix_rf_weighted <- confusionMatrix(
  data = test_predictions_rf_weighted,
  reference = test$nome_malattia_raggruppato
)
print(conf_matrix_rf_weighted)

# Plot variable importance
varImpPlot(rf_weighted)
```

```{r}
fm <- multinom(
  nome_malattia_raggruppato ~ fai_child_charact + fai_caregiving + fai_cure + 
    fai_intrapsych + fai_support + fai_hyper + sesso + eta_pz + legge_104,
  data = fai_clean_complete
)
summary(fm)
```
