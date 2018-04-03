### Modeling: Embedded Methods
# ---- Ridge Regression ----
# Lambda settings
ridge_lambdas <- 10^seq(-2, 3, by = .1)

# Model 
ridge_mod <- glmnet(x = data.matrix(train_split[, !"SalePrice", with = F]), y = train_split$SalePrice,
                    alpha = 0, lambda = ridge_lambdas)

# Cross-validation based model
ridge_cv_fit <- cv.glmnet(x = data.matrix(train_split[, !"SalePrice", with = F]), y = train_split$SalePrice,
                          alpha = 0, lambda = ridge_lambdas, nfolds = 20)

# Plot results
# plot(ridge_cv_fit)

# Assess best lambda
ridge_best_lam <- ridge_cv_fit$lambda.min
paste("Best Lambda value from CV=", ridge_best_lam)

# Predict values with best lambda
ridge_pred_best_lam = predict(ridge_mod, s = ridge_best_lam, data.matrix(validation_split[, !"SalePrice", with = F]))

# Save RMSE
ridge_best_lam_RMSE <- sqrt(mean((ridge_pred_best_lam - validation_split$SalePrice)^2))

# Print results 
paste("RMSE for lambda ", ridge_best_lam, " = ", ridge_best_lam_RMSE)

# Derive 1 SE Lambda
ridge_lam_1se <- ridge_cv_fit$lambda.1se
paste("Lambda 1se value from CV=", ridge_lam_1se)

# Predict values with 1 SE lambda
ridge_pred_1se_lam = predict(ridge_mod, s = ridge_lam_1se, data.matrix(validation_split[, !"SalePrice", with = F]))

# Save RMSE
ridge_1se_lam_RMSE <- sqrt(mean((ridge_pred_1se_lam - validation_split$SalePrice)^2))

# Print results
paste("RMSE for lambda ", ridge_lam_1se, " = ", ridge_1se_lam_RMSE)

# Plot important coefficiants
my_data_ridge = as.data.frame(cbind(predicted = ridge_pred_1se_lam, observed = validation_split$SalePrice))

ggplot(my_data_ridge, aes(my_data_ridge["1"], observed)) + geom_point() + geom_smooth(method = "lm") +
  scale_x_continuous(expand = c(0, 0)) + labs(x = "Predicted") + ggtitle('Ridge')

# # Print, plot variable importance
ridge_imp_best_lam <- varImp(ridge_mod, lambda = ridge_best_lam)
ridge_names_best_lam <- rownames(ridge_imp_best_lam)[order(ridge_imp_best_lam$Overall, decreasing=TRUE)]
ridge_importance_best_lam <- ridge_imp_best_lam[ridge_names_best_lam, ]

ridge_features_used <- data.frame(row.names = ridge_names_best_lam, ridge_importance_best_lam)
ridge_features_used

# ---- Lasso Regresion ----
# Set Lambda
lasso_lambdas <- 10^seq(-3, 3, by = .1)

# Model
lasso_mod <- glmnet(x = data.matrix(train_split[, !"SalePrice", with = F]), y = train_split$SalePrice,
                    alpha = 1, lambda = lasso_lambdas)

# Cross-validation based model
lasso_cv_fit <- cv.glmnet(x = data.matrix(train_split[, !"SalePrice", with = F]), y = train_split$SalePrice,
                          alpha = 1, lambda = lasso_lambdas, nfolds = 20)
# Plot Model
plot(lasso_cv_fit)

# Best Lambda
lasso_best_lam <- lasso_cv_fit$lambda.min
paste("Best Lambda value from CV=", lasso_best_lam)

# Prediction based on best lambda
lasso_pred_best_lam <- predict(lasso_mod, s = lasso_best_lam, data.matrix(validation_split[, !"SalePrice", with = F]))

# Save RMSE
lasso_best_lam_RMSE <- sqrt(mean((lasso_pred_best_lam - validation_split$SalePrice)^2))

# Print results
paste("RMSE for lambda ", lasso_best_lam, " = ", lasso_best_lam_RMSE)

# Derive 1 SE Lambda
lasso_lam_1se <- lasso_cv_fit$lambda.1se
paste("Lambda 1se value from CV=", lasso_lam_1se)

# Prediction based on best lambda
lasso_pred_1se <- predict(lasso_mod, s = lasso_lam_1se, data.matrix(validation_split[, !"SalePrice", with = F]))

# Save RMSE
lasso_1se_RMSE <- sqrt(mean((lasso_pred_1se - validation_split$SalePrice)^2))

# Print results 
paste("RMSE for lambda ", lasso_lam_1se, " = ", lasso_1se_RMSE)

# Plot important coefficients
my_data_lasso = as.data.frame(cbind(predicted = lasso_pred_best_lam, observed = validation_split$SalePrice))

ggplot(my_data_lasso, aes(my_data_lasso["1"], observed)) + geom_point() +geom_smooth(method="lm") +
  scale_x_continuous(expand = c(0,0)) + labs(x="Predicted") + ggtitle('Lasso')

# Print, plot variable importance
lasso_imp_best_lam <- varImp(lasso_mod, lambda = lasso_best_lam)
lasso_names_best_lam <- rownames(lasso_imp_best_lam)[order(lasso_imp_best_lam$Overall, decreasing=TRUE)]
lasso_importance_best_lamp <- lasso_imp_best_lam[lasso_names_best_lam, ]

lasso_features <- data.frame(row.names = lasso_names_best_lam, lasso_importance_best_lamp)
lasso_features_used <- rownames(lasso_imp_best_lam)[lasso_imp_best_lam$Overall>0]
lasso_features_used


# ---- Elastic Net ----
# Lambda grid
elastic_lambdas <- 10^seq(-3, 3, by = 0.1)

# Alpha gird
elastic_alphas <- seq(0, 1, by = 0.05)

# Search Grid
search_grid <- expand.grid(.alpha = elastic_alphas, .lambda = elastic_lambdas)

# Train Config for Elastic Net
train_control_config_elastic <- trainControl(method = "repeatedcv", 
                                            number = 10, 
                                            repeats = 3)

# Elastic Model 
elastic_cv_fit <- train(SalePrice ~ ., data = train_split, 
                        method = "glmnet", 
                        tuneGrid = search_grid, 
                        metric = "RMSE",
                        preProc = c("center", "scale"),
                        trControl = train_control_config_elastic)
# Inspect Model
attributes(elastic_cv_fit)

# Obtain best tuning parameters
elastic_cv_fit$bestTune

# Coffecients 
elastic_cv_mod <- elastic_cv_fit$finalModel
coef(elastic_cv_fit, s = elastic_cv_fit$bestTune$lambda)

# Final Model
elastic_mod <- glmnet(x = data.matrix(train_split[, !"SalePrice", with = F]), y = train_split$SalePrice,
                    alpha = elastic_cv_fit$bestTune$alpha, lambda = elastic_cv_fit$bestTune$lambda)

# Prediction based on best lambda
elasitc_pred <- predict(elastic_mod, s = elastic_cv_fit$bestTune$lambda, data.matrix(validation_split[, !"SalePrice", with = F]))

# Save RMSE
elastic_RMSE <- sqrt(mean((elasitc_pred - validation_split$SalePrice)^2))

# Print Results
paste("Elastic Net Linear Regression RMSE = ", elastic_RMSE)
