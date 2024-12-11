install.packages("SuperLearner")
install.packages(c("RhpcBLASctl"))
library(SuperLearner)
install.packages("kernlab")
library(kernlab)

variable_names <- data.frame(Variable = colnames(imputed_lsac))
write.csv(variable_names, "variable_names.csv", row.names = FALSE)

# Let's take out some transformed variables
imputed_lsac_SL <- imputed_lsac[, !names(imputed_lsac) %in% c("Npastweekdrinks")]

# Extract outcome from the dataframe
outcome = imputed_lsac_SL$risky_alc

# Create a dataframe to contain our explanatory variables.
data_SL = subset(imputed_lsac_SL, select = -risky_alc)

# Check structure of our dataframe.
str(data_SL)

# Set a seed for reproducibility in this random sampling.
set.seed(1)
train_obs = sample(nrow(data_SL), 0.75 * nrow(data_SL))
# X is our training sample.
x_train = data_SL[train_obs, ]
# Create a holdout set for evaluating model performance.
# Note: cross-validation is even better than a single holdout sample.
x_holdout = data_SL[-train_obs, ] # Our test set

y_train = outcome[train_obs] # Our outcome in the training set
y_holdout = outcome[-train_obs] # Outcome in the test set

# Review the outcome variable distribution.
table(y_train, useNA = "ifany")
table(y_holdout, useNA = "ifany")
class(imputed_lsac_SL$risky_alc) # should be numeric

# Fit superlearner model with multiple algorithms
set.seed(1)
sl_screening_combos = SuperLearner(Y = y_train, X = x_train, family = binomial(),
                            SL.library = list(
                              "SL.mean", 
                              "SL.glmnet", 
                              c("SL.glmnet", "screen.randomForest"), 
                              c("SL.glmnet", "screen.corP"),
                              "SL.ranger", 
                              c("SL.ranger", "screen.randomForest"),
                              c("SL.ranger", "screen.corP"),
                              "SL.ksvm",
                              c("SL.ksvm", "screen.randomForest"),
                              c("SL.ksvm", "screen.corP")
                            ))

sl_screening_combos

# Predict on holdout data and evaluate
pred = predict(sl_screening_combos, x_holdout, onlySL = TRUE)
pred_rocr_combos = ROCR::prediction(pred$pred, y_holdout)
auc = ROCR::performance(pred_rocr_combos, measure = "auc", x.measure = "cutoff")@y.values[[1]]
auc


# Fit ensemble with external cross validation
# This enables us to estimate the performance of the SL ensemble
# We generate a separate holdout sample that we don't use to fit the SuperLearner
# This allows it to be a good estimate of the SuperLearner's performance on unseen data
# Can get standard errors on the performance of the individual algorithms and compare to super learner

set.seed(1)

# Fit the SuperLearner.
# We need to use list() instead of c().
cv_super_learner = CV.SuperLearner(Y = y_train, X = x_train, family = binomial(),
                            V = 10,
                            parallel = "multicore",
                            SL.library = list("SL.mean", 
                                              "SL.glmnet", 
                                              c("SL.glmnet", "screen.randomForest"), 
                                              c("SL.glmnet", "screen.corP"),
                                              "SL.ranger", 
                                              c("SL.ranger", "screen.randomForest"),
                                              c("SL.ranger", "screen.corP"),
                                              "SL.ksvm",
                                              c("SL.ksvm", "screen.randomForest"),
                                              c("SL.ksvm", "screen.corP")))

summary(cv_super_learner)



sl_combo = SuperLearner(Y = y_train, X = x_train, family = binomial(),
                        SL.library = list("SL.mean", 
                                          "SL.glmnet", 
                                          c("SL.glmnet", "screen.randomForest"), 
                                          c("SL.glmnet", "screen.corP"),
                                          "SL.ranger", 
                                          c("SL.ranger", "screen.randomForest"),
                                          c("SL.ranger", "screen.corP"),
                                          "SL.ksvm",
                                          c("SL.ksvm", "screen.randomForest"),
                                          c("SL.ksvm", "screen.corP")))

sl_combo



#### Hyperparamter tuning ####
# Fit using the SuperLearner Package
# See what options we might like for mtry
# sqrt(p) is the default value of mtry for classification.
floor(sqrt(ncol(x_train))) # 14
# Let's try 3 multiplies of this default: 0.5, 1, and 2.
(mtry_seq = floor(sqrt(ncol(x_train)) * c(0.5, 1, 1.2)))


# Define the learners
ranger_learner = create.Learner(
  "SL.ranger",
  tune = list(
    num.trees = c(200, 500, 1000),
    mtry = mtry_seq,  # Define mtry_seq as needed, e.g., mtry_seq <- c(2, 3, 4)
    min.node.size = c(10, 25, 50)  # Experiment with different values
  )
)

# Check the created learner names
ranger_learner$names


glmnet_learner <- create.Learner(
  "SL.glmnet",
  tune = list(
    alpha = seq(0, 1, 0.1) # Tune alpha from 0 (Ridge) to 1 (LASSO), or Elastic Net in between
  )
)
glmnet_learner$names

ksvm_learner <- create.Learner(
  "SL.ksvm",
  tune = list(
    C = c(0.1, 1, 10),  # Regularization parameter
    sigma = c(0.01, 0.1, 1)  # Kernel width for RBF kernel
  )
)
ksvm_learner$names

# Specifying the SuperLearner library of candidate algorithms
sl.libr <- c("SL.mean",
             glmnet_learner$names,         # Tuned glmnet learners
             ksvm_learner$names,           # Tuned ksvm learners
             ranger_learner$names  # ranger with random forest screening
)


sl_for_imp = SuperLearner(Y = y_train, X = x_train, family = binomial(),
                        SL.library = sl.libr)

sl_for_imp # this took around 30 mins

pred = predict(sl_for_imp, x_holdout, onlySL = TRUE)  # Predict on the test set (x_holdout)
pred_rocr = ROCR::prediction(pred$pred, y_holdout)  # Evaluate using actual test labels (y_holdout)
auc = ROCR::performance(pred_rocr, measure = "auc", x.measure = "cutoff")@y.values[[1]]
auc # This gives 0.814

# Want to use multicore processing to speed up run time
(num_cores = RhpcBLASctl::get_num_cores())
options(mc.cores = 4)
getOption("mc.cores")
set.seed(1, "L'Ecuyer-CMRG")


cv_sl_tuned = CV.SuperLearner(Y = y_train, X = x_train, family = binomial(),
                            V = 10,
                            parallel = "multicore",
                            SL.library = sl.libr) # This took about 2.5 hours using 4 cores for parallel SuperLearner

summary(cv_sl_tuned)


#### Plots and AUCs ####
# Plot the performance with 95% CIs (use a better ggplot theme).
plot(cv_sl_tuned) + theme_bw()
library(ggplot2)

# Create a performance object for the ROC curve
perf_roc <- ROCR::performance(pred_rocr, measure = "tpr", x.measure = "fpr")

# Plot the ROC curve
plot(perf_roc, colorize = TRUE, main = paste("ROC Curve (AUC =", round(auc, 3), ")"))
abline(a = 0, b = 1, lty = 2, col = "gray")  # Add a diagonal line for reference

# Look at variable importance using iml
# Install iml if not already installed
if (!requireNamespace("iml", quietly = TRUE)) {
  install.packages("iml")
}

# Load the necessary libraries
library(SuperLearner)
library(iml)

# Define the prediction wrapper for SuperLearner
predict_function <- function(model, newdata) {
  predictions <- predict(model, newdata = newdata)$pred
  return(predictions)  # Directly return the probabilities
}

# Create the Predictor object
predictor <- Predictor$new(
  model = sl_for_imp,          # Your SuperLearner model
  data = x_holdout,        
  y = y_holdout,           # True labels
  predict.function = predict_function
)


# Compute feature importance
imp_final <- FeatureImp$new(
  predictor = predictor, 
  loss = "mae"  
)

# Plot the feature importance
plot(imp_final)
imp_final$results

imp_values_final <- imp_final$results

write.csv(imp_final$results, "imp_results_final.csv")

# Feature importance plot
# Reduce the number of features to the top 50, for interpretability of the plot
# Load necessary libraries
library(readxl)
library(ggplot2)

# Read the data from the Excel file
data_feat_imp <- read_excel("feature importance.xlsx")

# Display the first few rows of the data to confirm structure
print(head(data_feat_imp))

# Update the plot with new colors and smaller dots
plot_feat_imp <- ggplot(data_feat_imp, aes(x = reorder(feature, importance), y = importance)) +
  geom_point(color = "black", size = 2) +  # Change dot color to black and make them smaller
  geom_errorbar(aes(ymin = importance.05, ymax = importance.95), width = 0.2, color = "black") +  # Change error bar color to dark blue
  coord_flip() +
  labs(title = "Feature Importance with 5th and 95th Percentile Error Bars",
       x = "Feature",
       y = "Importance") +
  theme_minimal()

# Display the updated plot
print(plot_feat_imp)


# Individual plots for each top feature
for (feature in top_features) {
  single_eff <- FeatureEffect$new(predictor, feature = iccondb, method = "ale")
  plot(single_eff)
}

library(iml)


# ALEs
ale_plot <- FeatureEffect$new(
  predictor = predictor,
  feature = "gsle",  # Replace with the feature you want to analyze
  method = "ale"                 # Specify ALE method               
)

# Plot the ALE
plot(ale_plot)

ale_csep <- FeatureEffect$new(predictor = predictor, feature = "csep",  # Replace with the feature you want to analyze
  method = "ale")
plot(ale_csep)

# Top 10 features
ale_weeklydrink16 <- FeatureEffect$new(predictor = predictor, feature = "weeklydrink16",  # Replace with the feature you want to analyze
                              method = "ale")
plot(ale_weeklydrink16)

ale_cismale <- FeatureEffect$new(predictor = predictor, feature = "cismale",  # Replace with the feature you want to analyze
                                       method = "ale")
plot(ale_cismale)

ale_cannabis <- FeatureEffect$new(predictor = predictor, feature = "cannabis",  # Replace with the feature you want to analyze
                                 method = "ale")
plot(ale_cannabis)

ale_iccondb <- FeatureEffect$new(predictor = predictor, feature = "iccondb",  # Replace with the feature you want to analyze
                                  method = "ale")
plot(ale_iccondb)

ale_p2_alcp <- FeatureEffect$new(predictor = predictor, feature = "p2_alcp",  # Replace with the feature you want to analyze
                                 method = "ale")
plot(ale_p2_alcp)

ale_ipeermoral <- FeatureEffect$new(predictor = predictor, feature = "ipeermoral",  # Replace with the feature you want to analyze
                                 method = "ale")
plot(ale_ipeermoral)

ale_dacons <- FeatureEffect$new(predictor = predictor, feature = "dacons",  # Replace with the feature you want to analyze
                                    method = "ale")
plot(ale_dacons)

ale_p1_alcp <- FeatureEffect$new(predictor = predictor, feature = "p1_alcp",  # Replace with the feature you want to analyze
                                method = "ale")
plot(ale_p1_alcp)

ale_eaang <- FeatureEffect$new(predictor = predictor, feature = "eaang",  # Replace with the feature you want to analyze
                                 method = "ale")
plot(ale_eaang)

ale_f_finstress_cont <- FeatureEffect$new(predictor = predictor, feature = "f_finstress_cont",  # Replace with the feature you want to analyze
                               method = "ale")
plot(ale_f_finstress_cont)



