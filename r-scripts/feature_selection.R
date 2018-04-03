# ---- Feature Selection ----
# Analysis of Full LM Model
stat_full_lm <- summary(full_lm_mod)
stat_full_lm$coefficients
# Some p-values high, suggest variable not important.

### Find important features to use in model also to make polynomials 
# Chi-squared
chi_weights[order(chi_weights$attr_importance, decreasing = TRUE), ][1]
# Fetures that based threshold
chi_squared_features

# Information gain
info_gain_weights[order(info_gain_weights$attr_importance, decreasing = TRUE),][1]
# Fetures that based threshold
info_gain_features

# Forward Stepwise
paste("Features Selected" , forward_lm_mod$finalModel$formula[3])
# OverallQual + TotalSF + Neighborhood + OverallCond + BsmtFinType1 + GarageScore + LotArea +
# HouseAge + RelativeSize + SaleCondition + KitchenQual + BsmtFinSF1 + MSZoning + Heating + Functional

# Ridge Regression
ridge_features_used

# Lasso Regression
lasso_features_used

# ElasticNet Regression
elastic_features_used

# Selected Features
selected_features <- unique(c(chi_squared_features, info_gain_features))
selected_features

# New Lasso Regeression
# Set lambda setting
lasso_lambdas <- 10^seq(-3, 3, by = .1)

# Model
lasso_mod_sel_feat <- glmnet(x = data.matrix(train_split[, selected_features, with = F]), y = train_split$SalePrice,
                             alpha = 1, lambda = lasso_lambdas)

# Cross-validation based model with selected features
lasso_cv_fit_sel_feat <- cv.glmnet(x = data.matrix(train_split[, selected_features, with = F]), y = train_split$SalePrice,
                          alpha = 1, lambda = lasso_lambdas)


# Plot Model
plot(lasso_cv_fit_sel_feat)

# Lambda Selection
lasso_sel_feat_lam <- lasso_cv_fit_sel_feat$lambda.min
paste("Best Lambda value from CV=", lasso_sel_feat_lam)

# Prediction based on best lambda
lasso_sel_feat_pred <- predict(lasso_cv_fit_sel_feat, s = lasso_sel_feat_lam, data.matrix(validation_split[, selected_features, with = F]))

# Save RMSE
lasso_sel_feat_RMSE <- sqrt(mean((lasso_sel_feat_lam - validation_split$SalePrice)^2))

# Print results
paste("RMSE for lambda ", lasso_sel_feat_lam, " = ", lasso_sel_feat_RMSE)


