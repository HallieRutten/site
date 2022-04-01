library( tidyverse )
chronic <- read_csv( "chronic.csv")

# get a small subset for CV experiments
chronic_subset <- slice_sample( chronic, n=100 )

# k-fold cross-validation:
# (1) pick a number of folds, e.g., k=10
# (2) shuffle the data to reduce bias
# (3) divide the data into folds
# (4) train a model on (k-1) folds (training set) and test on the remaining fold (test a.k.a. holdout set); 
# do this train/test business k times
# (5) compare results / compute summary statistics for results

# (1) Use k = 10 -----

# (2) Shuffle the data -----
chronic_subset <- chronic_subset[ sample.int( nrow(chronic_subset)) , ]

# (3) Divide the data into folds -----
folds <- cut( 1:nrow(chronic_subset), breaks = 10, labels=1:10 )
chronic_subset <- chronic_subset %>%
  mutate( Fold = as.numeric( folds )) 

# more next time!
