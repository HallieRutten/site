# chronic sensitivity/specificity hw 4/7

library( tidyverse )
chronic <- read_csv( "chronic.csv")

chronic_subset <- slice_sample( chronic, n=1000 )
n_folds <- 10

chronic_subset <- chronic_subset[ sample.int( nrow(chronic_subset)) , ]

folds <- cut( 1:nrow(chronic_subset), breaks = n_folds, labels=1:n_folds )

chronic_subset <- chronic_subset %>%
  mutate( Fold = as.numeric( folds )) 

# sensitivity ----

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
  sensitivities <- c( sensitivities, sensitivity(test$Condition, test$Prediction) ) 
}

sensitivities
summary(sensitivities)

# specificity ----

specificity <- function( predicted_classes, true_classes ){
  idx <- which( true_classes == 0 )
  return( 1-( sum( predicted_classes[idx], na.rm=TRUE ) / length( idx ) ) )
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
  specificities <- c( specificities, specificity(test$Condition, test$Prediction) ) 
}

specificities
summary(specificities)

# mean of both sensitivity and specificity are close (approx .74 to .72)