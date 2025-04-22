library(ranger)
library(glmnet)
library(kernlab)
library(xgboost)
library(dplyr)
library(pROC)

sum(is.na(imputed_lsac)) # nothing missing
skim(imputed_lsac$caang) # standardised
class(imputed_lsac$caang)

variable_names <- data.frame(Variable = colnames(imputed_lsac))
write.csv(variable_names, "variable_names.csv", row.names = FALSE)

# Remove some transformed variables
imputed_lsac_main <- imputed_lsac[, !names(imputed_lsac) %in% c("Npastweekdrinks", "hicid")]

# Ensure risky_alc is a factor
imputed_lsac_main$risky_alc <- factor(imputed_lsac_main$risky_alc, levels = c(0, 1), labels = c("No", "Yes"))

# Create predictor list
predictors_main <- names(imputed_lsac_main)[names(imputed_lsac_main) != "risky_alc"]


# Initialize storage for main analysis
outer_folds <- createMultiFolds(MLDATA_modified_standardized$risky_alc, k = 5, times = 3)
ranger_auc_main <- numeric(length(outer_folds))
glmnet_auc_main <- numeric(length(outer_folds))
ksvm_auc_main <- numeric(length(outer_folds))
sl_auc_main <- numeric(length(outer_folds))
ranger_tunes_main <- list()
glmnet_tunes_main <- list()
ksvm_tunes_main <- list()
outer_results_main <- list()
outer_predictions_main <- list()
outer_true_values_main <- list()
importance_list_main <- list()


# Want to use multicore processing to speed up run time
(num_cores = RhpcBLASctl::get_num_cores())
options(mc.cores = 4)
getOption("mc.cores")
set.seed(1, "L'Ecuyer-CMRG")

# Define training control for tuning
ctrl <- trainControl(
  method = "cv",           # Cross-validation
  number = 10,              # 10-fold CV within training
  classProbs = TRUE,       # For ROC-AUC
  summaryFunction = twoClassSummary,  # ROC metric
  savePredictions = "final"
)

# Outer loop for main analysis
for (i in 1:length(outer_folds)) {
  cat("Outer fold", i, "of", length(outer_folds), "(main analysis)\n")
  
  train_indices <- outer_folds[[i]]
  train_data <- imputed_lsac_main[train_indices, ]
  test_data <- imputed_lsac_main[-train_indices, ]
  
  # Tuning with all predictors (use factor risky_alc)
  ranger_tune <- caret::train(
    x = train_data[, predictors_main],
    y = train_data$risky_alc, # Factor: "No"/"Yes"
    method = "ranger",
    trControl = ctrl,
    tuneGrid = expand.grid(
      mtry = c(floor(sqrt(length(predictors_main))), floor(length(predictors_main) / 3)),
      splitrule = "gini",
      min.node.size = c(5, 10)
    ),
    metric = "ROC",
    num.trees = 200,
    max.depth = 10
  )
  ranger_tunes_main[[i]] <- ranger_tune$bestTune
  
  glmnet_tune <- caret::train(
    x = train_data[, predictors_main],
    y = train_data$risky_alc, # Factor
    method = "glmnet",
    trControl = ctrl,
    tuneGrid = expand.grid(
      alpha = seq(0, 1, by = 0.2),
      lambda = seq(0.01, 0.05, length = 10)
    ),
    metric = "ROC"
  )
  glmnet_tunes_main[[i]] <- glmnet_tune$bestTune
  
  ksvm_tune <- caret::train(
    x = train_data[, predictors_main],
    y = train_data$risky_alc, # Factor
    method = "svmRadial",
    trControl = ctrl,
    tuneGrid = expand.grid(
      C = c(0.1, 1, 10),
      sigma = c(0.01, 0.1, 1)
    ),
    metric = "ROC"
  )
  ksvm_tunes_main[[i]] <- ksvm_tune$bestTune
  
  # SuperLearner wrappers for main analysis
  SL.ranger.tuned.main <- function(Y, X, newX, family, ...) {
    fit <- ranger::ranger(
      y = factor(Y, levels = c(0, 1), labels = c("No", "Yes")),
      x = X, 
      mtry = ranger_tunes_main[[i]]$mtry, 
      splitrule = ranger_tunes_main[[i]]$splitrule,
      min.node.size = ranger_tunes_main[[i]]$min.node.size, 
      num.trees = 200, 
      max.depth = 10, 
      probability = TRUE
    )
    pred <- predict(fit, data = newX)$predictions[, 2]
    list(pred = pred, fit = fit)
  }
  
  SL.glmnet.tuned.main <- function(Y, X, newX, family, ...) {
    fit <- glmnet::glmnet(
      x = as.matrix(X), 
      y = Y,
      family = "binomial",
      alpha = glmnet_tunes_main[[i]]$alpha, 
      lambda = glmnet_tunes_main[[i]]$lambda,
      maxit = 10000000
    )
    pred <- predict(fit, newx = as.matrix(newX), type = "response", s = glmnet_tunes_main[[i]]$lambda)
    list(pred = pred, fit = fit)
  }
  
  SL.ksvm.tuned.main <- function(Y, X, newX, family, ...) {
    fit <- kernlab::ksvm(
      x = as.matrix(X), 
      y = factor(Y, levels = c(0, 1), labels = c("No", "Yes")),
      kernel = "rbfdot", 
      type = "C-svc",
      C = ksvm_tunes_main[[i]]$C, 
      kpar = list(sigma = ksvm_tunes_main[[i]]$sigma), 
      prob.model = TRUE
    )
    pred <- predict(fit, newdata = as.matrix(newX), type = "probabilities")[, 2]
    list(pred = pred, fit = fit)
  }
  
  # SuperLearner (use numeric risky_alc)
  sl_model <- SuperLearner(
    Y = as.numeric(train_data$risky_alc == "Yes"), # Convert factor to 0/1
    X = train_data[, predictors_main],
    newX = test_data[, predictors_main],
    SL.library = c("SL.ranger.tuned.main", "SL.glmnet.tuned.main", "SL.ksvm.tuned.main"),
    method = "method.NNLS",
    verbose = TRUE
  )
  sl_pred <- sl_model$SL.predict
  sl_auc_main[i] <- roc(as.numeric(test_data$risky_alc == "Yes"), sl_pred, quiet = TRUE)$auc
  sl_weights_main <- sl_model$coef
  names(sl_weights_main) <- gsub("_All", "", names(sl_weights_main))
  cat("Weights for Fold", i, "(main analysis):\n")
  print(sl_weights_main)
  
  # Evaluate individual algorithms
  ranger_fit <- ranger::ranger(
    y = train_data$risky_alc,
    x = train_data[, predictors_main],
    mtry = ranger_tunes_main[[i]]$mtry,
    splitrule = ranger_tunes_main[[i]]$splitrule,
    min.node.size = ranger_tunes_main[[i]]$min.node.size,
    num.trees = 200,
    max.depth = 10,
    probability = TRUE
  )
  ranger_pred <- predict(ranger_fit, data = test_data[, predictors_main])$predictions[, 2]
  ranger_auc_main[i] <- roc(as.numeric(test_data$risky_alc == "Yes"), ranger_pred, quiet = TRUE)$auc
  
  glmnet_fit <- glmnet::glmnet(
    x = as.matrix(train_data[, predictors_main]),
    y = as.numeric(train_data$risky_alc == "Yes"),
    family = "binomial",
    alpha = glmnet_tunes_main[[i]]$alpha,
    lambda = glmnet_tunes_main[[i]]$lambda,
    maxit = 10000000
  )
  glmnet_pred <- predict(glmnet_fit, newx = as.matrix(test_data[, predictors_main]), type = "response", s = glmnet_tunes_main[[i]]$lambda)
  glmnet_auc_main[i] <- roc(as.numeric(test_data$risky_alc == "Yes"), glmnet_pred, quiet = TRUE)$auc
  
  ksvm_fit <- kernlab::ksvm(
    x = as.matrix(train_data[, predictors_main]),
    y = train_data$risky_alc,
    kernel = "rbfdot",
    type = "C-svc",
    C = ksvm_tunes_main[[i]]$C,
    kpar = list(sigma = ksvm_tunes_main[[i]]$sigma),
    prob.model = TRUE
  )
  ksvm_pred <- predict(ksvm_fit, newdata = as.matrix(test_data[, predictors_main]), type = "probabilities")[, 2]
  ksvm_auc_main[i] <- roc(as.numeric(test_data$risky_alc == "Yes"), ksvm_pred, quiet = TRUE)$auc
  
  # Importance calculations
  ranger_fit <- ranger(
    y = train_data$risky_alc,
    x = train_data[, predictors_main],
    mtry = ranger_tunes_main[[i]]$mtry,
    splitrule = ranger_tunes_main[[i]]$splitrule,
    min.node.size = ranger_tunes_main[[i]]$min.node.size,
    num.trees = 200,
    max.depth = 10,
    probability = TRUE,
    importance = "permutation"
  )
  ranger_imp <- ranger_fit$variable.importance
  
  glmnet_fit <- glmnet(
    x = as.matrix(train_data[, predictors_main]),
    y = as.numeric(train_data$risky_alc == "Yes"),
    family = "binomial",
    alpha = glmnet_tunes_main[[i]]$alpha,
    lambda = glmnet_tunes_main[[i]]$lambda,
    maxit = 10000000
  )
  glmnet_imp <- abs(coef(glmnet_fit))[-1]
  
  ksvm_fit <- kernlab::ksvm(
    x = as.matrix(train_data[, predictors_main]),
    y = train_data$risky_alc,
    kernel = "rbfdot",
    type = "C-svc",
    C = ksvm_tunes_main[[i]]$C,
    kpar = list(sigma = ksvm_tunes_main[[i]]$sigma),
    prob.model = TRUE
  )
  base_pred <- predict(ksvm_fit, newdata = as.matrix(test_data[, predictors_main]), type = "probabilities")[, 2]
  base_auc <- roc(as.numeric(test_data$risky_alc == "Yes"), base_pred, quiet = TRUE)$auc
  ksvm_imp <- sapply(predictors_main, function(pred) {
    perm_aucs <- replicate(5, {
      permuted_data <- test_data
      permuted_data[[pred]] <- sample(permuted_data[[pred]])
      perm_pred <- predict(ksvm_fit, newdata = as.matrix(permuted_data[, predictors_main]), type = "probabilities")[, 2]
      roc(as.numeric(test_data$risky_alc == "Yes"), perm_pred, quiet = TRUE)$auc
    })
    mean(base_auc - perm_aucs)
  })
  cat("KSVM Base AUC for Fold", i, "(main analysis):", base_auc, "\n")
  
  # Combine importance (raw)
  imp_df_main <- data.frame(
    Feature = predictors_main,
    Ranger = ranger_imp[match(predictors_main, names(ranger_imp))],
    GLMNET = glmnet_imp[match(predictors_main, rownames(coef(glmnet_fit))[-1])],
    KSVM = ksvm_imp
  )
  imp_df_main$Ranger[is.na(imp_df_main$Ranger)] <- 0
  imp_df_main$GLMNET[is.na(imp_df_main$GLMNET)] <- 0
  imp_df_main$KSVM[is.na(imp_df_main$KSVM)] <- 0
  
  weighted_imp_main <- imp_df_main %>%
    mutate(
      Weighted = coalesce(sl_weights_main["SL.ranger.tuned.main"], 0) * Ranger +
        coalesce(sl_weights_main["SL.glmnet.tuned.main"], 0) * GLMNET +
        coalesce(sl_weights_main["SL.ksvm.tuned.main"], 0) * KSVM
    )
  
  importance_list_main[[i]] <- weighted_imp_main
  outer_results_main[[i]] <- sl_model
  outer_predictions_main[[i]] <- sl_pred
  outer_true_values_main[[i]] <- test_data$risky_alc
}


# Summarize AUCs
auc_summary_main <- data.frame(
  Model = c("SuperLearner", "Ranger", "GLMNET", "KSVM"),
  Mean_AUC = c(mean(sl_auc_main, na.rm = TRUE), mean(ranger_auc_main, na.rm = TRUE), 
               mean(glmnet_auc_main, na.rm = TRUE), mean(ksvm_auc_main, na.rm = TRUE)),
  SD_AUC = c(sd(sl_auc_main, na.rm = TRUE), sd(ranger_auc_main, na.rm = TRUE), 
             sd(glmnet_auc_main, na.rm = TRUE), sd(ksvm_auc_main, na.rm = TRUE))
)
cat("AUC Comparison Across Models (Main Analysis):\n")
print(auc_summary_main, 4)
write.csv(auc_summary_main, "auc_summary_main.csv", row.names = FALSE)

# Calculate AUC per fold
auc_per_fold_main <- sapply(1:length(outer_folds), function(i) {
  pred <- outer_predictions_main[[i]]
  true <- as.numeric(outer_true_values_main[[i]] == "Yes") # "No" = 0, "Yes" = 1
  roc_obj <- roc(true, pred, quiet = TRUE)
  as.numeric(roc_obj$auc)
})
cat("AUC per fold (main analysis):\n")
print(round(auc_per_fold_main, 4))
write.csv(data.frame(Fold = 1:15, AUC = auc_per_fold_main), "auc_per_fold_main.csv", row.names = FALSE)

# Extract and save weights
weights_all_folds_main <- lapply(1:length(outer_results_main), function(i) {
  sl_model <- outer_results_main[[i]]
  if (!is.null(sl_model) && !is.null(sl_model$coef)) {
    coef <- sl_model$coef
    names(coef) <- gsub("_All", "", names(coef))
    return(coef)
  } else {
    return(rep(NA, 3))
  }
})
weights_df_main <- as.data.frame(do.call(rbind, weights_all_folds_main))
colnames(weights_df_main) <- c("SL.ranger.tuned.main", "SL.glmnet.tuned.main", "SL.ksvm.tuned.main")
weights_df_main$Fold <- 1:nrow(weights_df_main)
weights_df_main <- weights_df_main[, c("Fold", "SL.ranger.tuned.main", "SL.glmnet.tuned.main", "SL.ksvm.tuned.main")]
cat("SuperLearner Weights for All 15 Folds (Main Analysis):\n")
print(weights_df_main)
write.csv(weights_df_main, "weights_main.csv", row.names = FALSE)

# Mean weights
mean_weights_main <- colMeans(weights_df_main[, -1], na.rm = TRUE)
cat("\nMean Weights Across Folds (Main Analysis):\n")
print(round(mean_weights_main, 6))

# Normalization of feature importance
normalize_imp <- function(x) {
  if (max(x, na.rm = TRUE) == min(x, na.rm = TRUE)) return(rep(0, length(x)))
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# Validate importance inputs
if (length(importance_list_main) != 15) {
  stop("importance_list_main should have 15 folds, got ", length(importance_list_main))
}
if (length(outer_results_main) != 15) {
  stop("outer_results_main should have 15 folds, got ", length(outer_results_main))
}
if (!all(sapply(importance_list_main, function(x) identical(x$Feature, importance_list_main[[1]]$Feature)))) {
  stop("Feature names mismatch across folds")
}

# Normalize importance scores
importance_list_main_normalized <- lapply(1:length(importance_list_main), function(i) {
  imp_df <- importance_list_main[[i]]
  
  imp_df$Ranger <- normalize_imp(imp_df$Ranger)
  imp_df$GLMNET <- normalize_imp(imp_df$GLMNET)
  imp_df$KSVM <- normalize_imp(imp_df$KSVM)
  
  weights <- outer_results_main[[i]]$coef
  names(weights) <- gsub("_All", "", names(weights))
  
  imp_df$Weighted <- with(imp_df, 
                          coalesce(weights["SL.ranger.tuned.main"], 0) * Ranger +
                            coalesce(weights["SL.glmnet.tuned.main"], 0) * GLMNET +
                            coalesce(weights["SL.ksvm.tuned.main"], 0) * KSVM
  )
  
  imp_df
})

# Aggregate normalized importance
final_importance_main_normalized <- importance_list_main_normalized[[1]] %>%
  dplyr::select(Feature) %>%
  dplyr::mutate(
    Weighted_Mean = rowMeans(sapply(importance_list_main_normalized, function(x) x$Weighted)),
    Weighted_SD = apply(sapply(importance_list_main_normalized, function(x) x$Weighted), 1, sd)
  ) %>%
  dplyr::arrange(desc(Weighted_Mean))

# Save normalized results
write.csv(final_importance_main_normalized, "feature_importance_main_normalized.csv", row.names = FALSE)

# Print top predictors
cat("Top 20 Normalized Feature Importances (Main Analysis):\n")
print(head(final_importance_main_normalized, 20))
cat("Top 20 Raw Feature Importances (Main Analysis):\n")
print(head(final_importance_main, 20))




