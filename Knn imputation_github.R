# K-nearest neighbour imputation
install.packages("VIM")
library(VIM)

# Need to scale before imputing
# Check what format variables are in
data.frame(Variable = names(lsac_analysis), Class = sapply(lsac_analysis, class))
# All numeric


# Identify continuous and binary variables
continuous_vars <- names(lsac_analysis)[sapply(lsac_analysis, function(x) is.numeric(x) && length(unique(na.omit(x))) > 2)]
binary_vars <- names(lsac_analysis)[sapply(lsac_analysis, function(x) is.numeric(x) && length(unique(na.omit(x))) == 2)]

# Create a copy of the dataset
lsac_analysis_scaled <- lsac_analysis

# Scale continuous variables only
lsac_analysis_scaled[continuous_vars] <- scale(lsac_analysis[continuous_vars])

# For KNN, all vars need to be numeric
# Check the structure of the new dataframe
str(lsac_analysis_scaled)
skim(lsac_analysis$cDV)
mean(lsac_analysis_scaled$cDV)
tab1(lsac_analysis_scaled$cDV)
tab1(lsac_analysis$cP1SE)
options(scipen = 999)
options(digits = 5)

# Perform KNN imputation using VIM package
time_taken <- system.time({
  imputed_lsac <- kNN(lsac_analysis_scaled, k = 5, imp_var = FALSE)
})

# Check there's no missing
sum(is.na(imputed_lsac)) 

# Check outcome prevalence
tab1(imputed_lsac$risky_alc)
tab1(lsacwide$risky_alc)

write_sav(imputed_lsac, "imputed_lsac.sav")

# To scale back into original units
# Extract scaling attributes directly from the original scaling operation
means <- attr(scale(lsac_analysis[continuous_vars], center = TRUE, scale = TRUE), "scaled:center")
sds <- attr(scale(lsac_analysis[continuous_vars], center = TRUE, scale = TRUE), "scaled:scale")

# Reverse scaling using these attributes
numeric_vars_original_scale <- sweep(imputed_lsac[continuous_vars], 2, sds, '*')  # Multiply by SD
numeric_vars_original_scale <- sweep(numeric_vars_original_scale, 2, means, '+')  # Add the mean

# Replace the scaled variables in the dataset with the original scale
imputed_lsac[continuous_vars] <- numeric_vars_original_scale

# Check
skim(lsac_analysis$caang)
skim(imputed_lsac$caang)
tab1(imputed_lsac$risky_alc)
tab1(imputed_lsac$cDV)
tab1(imputed_lsac$h_mum_interest_ed)
