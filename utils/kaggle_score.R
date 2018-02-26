kaggle_score <- function(pred, actual) {
  if (length(colnames(pred)) != length(colnames(actual))) warning("Different number of columns between pred and actual")
  if (!all(colnames(pred) %in% colnames(actual))) warning("actual does not contain all columns names from pred")
  
  common_cols <- intersect(colnames(pred), colnames(actual))
  message(paste("Doing ROC AUC check on the following columns", paste(common_cols, collapse = ",")))
  
  aucs <- vector("numeric", length(common_cols))
  
  for (i in seq_along(common_cols)) {
    col_n <- common_cols[i]
    toxic.pred <- ROCR::prediction(pred[, col_n], actual[, col_n])
    aucs[i] <- ROCR::performance(toxic.pred, measure = "auc")@y.values[[1]]
    names(aucs)[i] <- col_n
  }
  
  print(aucs)
  mean(aucs)
}

