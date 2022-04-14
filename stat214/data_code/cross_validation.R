library( tidyverse )
chronic <- read_csv( "chronic.csv")

# get a small subset for CV experiments
chronic_subset <- slice_sample( chronic, n=1000 )

# k-fold cross-validation:
# (1) pick a number of folds, e.g., k=10
# (2) shuffle the data to reduce bias
# (3) divide the data into folds
# (4) train a model on (k-1) folds (training set) and test on the remaining fold (test a.k.a. holdout set); 
# do this train/test business k times
# (5) compare results / compute summary statistics for results

# (1) Use n_folds = 10 and  do n_runs = 500 runs -----
n_folds <- 10
n_runs <- 500

# metrics -----

# true_classes and predicted_classes are
# vectors of 0s and 1s of equal length
# overall accuracy -----
accuracy <- function( predicted_classes, true_classes ){
  return( length(which( true_classes == predicted_classes )) / length( true_classes ) )
}

# sensitivity -----
sensitivity <- function( predicted_classes, true_classes ){
  idx <- which( true_classes == 1 )
  return( sum(predicted_classes[idx] ) / length( idx ) )
}

# specificity -----
specificity <- function( predicted_classes, true_classes ){
  idx <- which( true_classes == 0 )
  return( 1 - (sum(predicted_classes[idx] ) / length( idx )) ) 
}

metrics <- data.frame( accuracy=c(), sensitivity=c(), specificity=c() )

for( j in 1:n_runs ){
  
  # (2) Shuffle the data -----
  chronic_subset <-
    chronic_subset[sample.int(nrow(chronic_subset)) ,]
  
  # (3) Divide the data into folds -----
  folds <-
    cut(1:nrow(chronic_subset),
        breaks = n_folds,
        labels = 1:n_folds)
  chronic_subset <- chronic_subset %>%
    mutate(Fold = as.numeric(folds))
  
  for (i in 1:n_folds) {
    idx <- which(chronic_subset$Fold == i)
    test <- chronic_subset[idx, ]
    train <- chronic_subset[-idx, ]
    fit <- glm(Condition ~ Age, data = train, family = "binomial")
    print(summary(fit))
    test_predictions <-
      predict(fit, newdata = test, type = "response")
    test <- test %>%
      mutate(Prediction = as.numeric(test_predictions >= 0.585))
    metrics <- rbind(
      metrics,
      data.frame(
        accuracy = accuracy(test$Condition, test$Prediction),
        sensitivity = sensitivity(test$Prediction, test$Condition),
        specificity = specificity(test$Prediction, test$Condition)
      )
    )
  }
}

summary(metrics)

ggplot( metrics %>% pivot_longer(accuracy:specificity), aes(value) ) +
  geom_histogram() + 
  facet_wrap(~name) + 
  xlim(0,1)
