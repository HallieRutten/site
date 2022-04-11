# Homework

# Write functions to compute sensitivity (true positives) and specificity (true negatives)

library( tidyverse )

library(readr)

chronic <- read_csv("Desktop/site-master/stat214/data_code/chronic.csv")

chronic_subset <- slice_sample( chronic, n=1000 )

# (1) Use n_folds = 10 -----

n_folds <- 10

# (2) Shuffle the data -----

chronic_subset <- chronic_subset[ sample.int( nrow(chronic_subset)) , ]

# (3) Divide the data into folds -----

folds <- cut( 1:nrow(chronic_subset), breaks = n_folds, labels=1:n_folds )

chronic_subset <- chronic_subset %>%
  
  mutate( Fold = as.numeric( folds ))


# Sensitivity 

sensitivity <- function( predicted_classes, true_classes ){
  
  idx <- which( true_classes == 1 )
  
  return( sum( predicted_classes[idx], na.rm=TRUE ) / length( idx ) )

}
  sensitivities <- c()

for(i in 1:n_folds) {
  
  idx <- which(chronic_subset$Fold == i)
  
  test <- chronic_subset[idx,]
  
  train <- chronic_subset[-idx,]
  
  fit <- glm(Condition ~ Age, data = train, family = "binomial")
  
  print( summary(fit) )
  
  test_predictions <- predict(fit, newdata = test, type = "response")
  
  test <- test %>%
    
    mutate(Prediction = as.numeric(test_predictions >= 0.5))
  
  sensitivities <- c( sensitivities, sensitivity(test$Condition, test$Prediction) )}

sensitivities

summary( sensitivities )


# Specificity 

specificity <- function( predicted_classes, true_classes ){
  
  idx <- which( true_classes == 0 )
  
  return( 1-(sum( predicted_classes[idx], na.rm=TRUE ) / length( idx ) ) )
  
}
specificities <- c()

for(i in 1:n_folds) {
  
  idx <- which(chronic_subset$Fold == i)
  
  test <- chronic_subset[idx,]
  
  train <- chronic_subset[-idx,]
  
  fit <- glm(Condition ~ Age, data = train, family = "binomial")
  
  print( summary(fit) )
  
  test_predictions <- predict(fit, newdata = test, type = "response")
  
  test <- test %>%
    
    mutate(Prediction = as.numeric(test_predictions >= 0.5))
  
  specificities <- c( specificities, specificity(test$Condition, test$Prediction) )}

specificities

summary( specificities )
