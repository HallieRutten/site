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

# (1) Use n_folds = 10 -----
n_folds <- 10

# (2) Shuffle the data -----
chronic_subset <- chronic_subset[ sample.int( nrow(chronic_subset)) , ]

# (3) Divide the data into folds -----
folds <- cut( 1:nrow(chronic_subset), breaks = n_folds, labels=1:n_folds )
chronic_subset <- chronic_subset %>%
  mutate( Fold = as.numeric( folds )) 

# ground_truth and predicted_classes are
# vectors of 0s and 1s of equal length
accuracy <- function( ground_truth, predicted_classes ){
  return( length(which( ground_truth == predicted_classes )) / length( ground_truth ) )
}

# for Thursday:
# write functions to compute sensitivity and specificity 

accuracies <- c() 
for(i in 1:n_folds) {
  idx <- which(chronic_subset$Fold == i)
  test <- chronic_subset[idx,]
  train <- chronic_subset[-idx,]
  fit <- glm(Condition ~ Age, data = train, family = "binomial")
  print( summary(fit) )
  test_predictions <- predict(fit, newdata = test, type = "response")
  test <- test %>%
    mutate(Prediction = as.numeric(test_predictions >= 0.5))
  accuracies <- c( accuracies, accuracy(test$Condition, test$Prediction) ) 
}

accuracies
summary( accuracies )
