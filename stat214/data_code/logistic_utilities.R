###############################################################################
# convenience methods for analyzing results assume that: 
#   - true_classes is binary numeric (0's and 1's)
#   - predicted_classes is binary numeric (0's and 1's)
#   - predicted_responses has probabilities (type="response" from predict.glm)
###############################################################################

accuracy <- function( predicted_classes, true_classes ){ 
  return( mean( predicted_classes == true_classes, na.rm=TRUE))
}

sensitivity <- function( predicted_classes, true_classes ){
  idx <- which( true_classes == 1 )
  return( sum( predicted_classes[idx], na.rm=TRUE ) / length( idx ) )
}

specificity <- function( predicted_classes, true_classes ){
  idx <- which( true_classes == 0 )
  return( ( length(idx) - sum( predicted_classes[idx], na.rm=TRUE ) ) / length( idx ) )
}

rocr_auc <- function( predicted_responses, true_classes, plot_roc = TRUE, plot_title="ROC curve" ){
  if( any( is.na( predicted_responses ) ) ){
    idx <- which(is.na( predicted_responses))
    pred <- prediction( predicted_responses[-idx], true_classes[-idx] )
  } else {
    pred <- prediction( predicted_responses, true_classes )
  }
  perf <- performance( pred, "tpr", "fpr" )
  if( plot_roc ){
    plot( perf, colorize=TRUE, main = plot_title )
  }
  auc <- performance( pred, "auc" )
  return( auc@y.values[[1]] )
}

proc_auc <- function( predicted_responses, true_classes, plot_roc = TRUE, plot_title="ROC curve" ){
  if( any( is.na( predicted_responses ) ) ){
    idx <- which(is.na( predicted_responses))
    pred <- prediction( predicted_responses[-idx], true_classes[-idx] )
  } else {
    pred <- prediction( predicted_responses, true_classes )
  }
  roc.obj <- roc( true_classes, predicted_responses, plot=plot_roc, percent=plot_roc, print.auc=plot_roc, main=plot_title)
  ci_auc <- ci.auc( roc.obj )
  auc <- auc( roc.obj ) 
  return( c( ci_auc[1], auc[1], ci_auc[3] ) )
}

metrics <- function( predicted_classes, predicted_responses, true_classes, plot_roc= TRUE, plot_title="ROC curve" ){
  ci = proc_auc( predicted_responses, true_classes, plot_roc, plot_title ) 
  return( data.frame( Accuracy = accuracy( predicted_classes, true_classes ),
                      Sensitivity = sensitivity( predicted_classes, true_classes ),
                      Specificity = specificity( predicted_classes, true_classes ),
                      # ROCR = rocr_auc( predicted_responses, true_classes, plot_roc, plot_title ),
                      AUC_lower = ci[1],
                      AUC = ci[2] ,
                      AUC_upper = ci[3] ) )
}
