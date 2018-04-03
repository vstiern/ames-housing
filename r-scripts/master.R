### Title: Kaggle Housing Assignment -> Predict Sales Price
### Athour: Vilhem Stiernstedt
## Date: 2018-02-17

### Set Seed
seed_setting <- set.seed(1)
### Load Scripts
source("libraries.R") # Load Libraries
source("data_load_cleaning.R") # Load data cleaning (data inspection, NA and Outlier treatment)
source("outliers.R") # Load outlier treatment
# source("multivariate_analysis.R") # Load Univariate / Multivariate Analysis
source("feature_creation.R") # Load newly created features
source("data_transformation.R") # Load transformations (Log transformation)
source("train_validation_split.R") # Load train-validation split
source("baseline_lm.R") # Run LM with all features -> use RMSE as baseline
source("filter_methods.R") # Load filter methods (Chi-squared Selection and information gain)
source("wrapper_methods.R") # Load wrapper methods (Stepwise - back/forward)
source("embedded_methods.R") # Load embedded methods (Ridge and Lasso regression)
#source('feature_selection.R') # Load feature selection conclusion

# Results 
### Baseline LM 
paste("Full Linear Regression RMSE = ", full_lm_mod_RMSE)

### Chi-squared selection - LM 
paste("Chi-Squared Filtered Linear Regression RMSE = ", chi_squared_lm_mod_RMSE)
### Information Gain selection - LM 
paste("IG Filtered Linear Regression RMSE = ", info_gain_lm_mod_RMSE)

### Backward Stepwise selection - LM 
paste("Backward Linear Regression RMSE = ", backward_lm_mod_RMSE)
### Forward Stepwise selection - LM
paste("Forward Linear Regression RMSE = ", forward_lm_mod_RMSE)

### Ridge Regression
paste("RMSE for lambda ", ridge_best_lam, " = ", ridge_best_lam_RMSE)
paste("RMSE for lambda ", ridge_lam_1se, " = ", ridge_1se_lam_RMSE)

### Lasso Regression 
paste("RMSE for lambda ", lasso_best_lam, " = ", lasso_best_lam_RMSE)
paste("RMSE for lambda ", lasso_lam_1se, " = ", lasso_1se_RMSE)

# Function to calculate the RMSE scores for a different validations splits
embedded_best_lam_RMSE <- function(seed_iterations = 3) {
  lasso_RMSE_list <- c() #empty list to add RMSE scores
  ridge_RMSE_list <- c() #empty list to add RMSE scores
  seed_list <- c()
  
  # Get RMSE for selected number of seed iterations in range 1:100
  for (i in sample(100, seed_iterations)) {
    set.seed(i)
    seed_list <- c(seed_list, i)
    source("train_validation_split.R") # Load train-validation split
    source("embedded_methods.R") # Load embedded methods (Ridge and Lasso regression)
    lasso_RMSE_list <- c(lasso_RMSE_list, lasso_best_lam_RMSE[1]) # add RMSE to list
    ridge_RMSE_list <- c(ridge_RMSE_list, ridge_best_lam_RMSE[1]) # add RMSE to list
  }
  print(paste("Lasso min RMSE:", min(lasso_RMSE_list, na.rm = T))) # calc min of all RMSEs
  print(paste("Lasso mean RMSE:", mean(lasso_RMSE_list, na.rm = T))) # calc mean of all RMSEs
  print(paste("Lasso max RMSE:", max(lasso_RMSE_list, na.rm = T))) # calc max of all RMSEs
  print(paste("Ridge min RMSE:", min(ridge_RMSE_list, na.rm = T))) # calc min of all RMSEs
  print(paste("Ridge mean RMSE:", mean(ridge_RMSE_list, na.rm = T))) # calc mean of all RMSEs
  print(paste("Ridge max RMSE:", max(ridge_RMSE_list, na.rm = T))) # calc max of all RMSEs
  print("Seed numbers used:")
  print(seed_list) # print seed number tested
}  

# Run function
embedded_best_lam_RMSE(10)

# Function to calculate the RMSE scores for a different validations splits
embedded_1se_lam_RMSE <- function(seed_iterations = 3) {
  lasso_RMSE_list <- c() #empty list to add RMSE scores
  ridge_RMSE_list <- c() #empty list to add RMSE scores
  seed_list <- c()
  
  # Get RMSE for selected number of seed iterations in range 1:100
  for (i in sample(100, seed_iterations)) {
    set.seed(i)
    seed_list <- c(seed_list, i)
    source("train_validation_split.R") # Load train-validation split
    source("embedded_methods.R") # Load embedded methods (Ridge and Lasso regression)
    lasso_RMSE_list <- c(lasso_RMSE_list, lasso_1se_RMSE[1]) # add RMSE to list
    ridge_RMSE_list <- c(ridge_RMSE_list, ridge_1se_lam_RMSE[1]) # add RMSE to list
  }
  print(paste("Lasso min RMSE:", min(lasso_RMSE_list, na.rm = T))) # calc min of all RMSEs
  print(paste("Lasso mean RMSE:", mean(lasso_RMSE_list, na.rm = T))) # calc mean of all RMSEs
  print(paste("Lasso max RMSE:", max(lasso_RMSE_list, na.rm = T))) # calc max of all RMSEs
  print(paste("Ridge min RMSE:", min(ridge_RMSE_list, na.rm = T))) # calc min of all RMSEs
  print(paste("Ridge mean RMSE:", mean(ridge_RMSE_list, na.rm = T))) # calc mean of all RMSEs
  print(paste("Ridge max RMSE:", max(ridge_RMSE_list, na.rm = T))) # calc max of all RMSEs
  print("Seed numbers used:")
  print(seed_list) # print seed number tested
}  

# Run function
embedded_1se_lam_RMSE(20)

