### Feature Selection : Filter Methods
# ---- Chi-squared Selection ----

### Calculate ChiSquared based on training data
chi_weights <- data.frame(chi.squared(SalePrice ~ ., dt_train))

# Display variables ranked by importance
chi_weights$feature <- rownames(chi_weights)
chi_weights[order(chi_weights$attr_importance, decreasing = TRUE), ][1]

# Set importance threshold 
chi_thres <- 0.37

# Subset name features thats pass threshold
chi_squared_features <- chi_weights$feature[chi_weights$attr_importance >= chi_thres]

# Train Config for LM
train_control_config_filter <- trainControl(method = "repeatedcv", 
                                     number = 10, 
                                     repeats = 3,
                                     returnResamp = "all")

### Compute new LM with subsetted features
chi_squared_lm_mod <- train(SalePrice ~ ., data = train_split[, c(chi_squared_features, "SalePrice"), with = F], 
                            method = "lm", 
                            metric = "RMSE",
                            preProc = c("center", "scale"),
                            trControl = train_control_config_filter)

# Uniate levels from model with validation split
for (x in names(validation_split)) {
  chi_squared_lm_mod$xlevels[[x]] <- union(chi_squared_lm_mod$xlevels[[x]], levels(validation_split[[x]]))
}

# Make predictions for SalePrice in validation data
chi_squared_lm_mod_pred <- predict(chi_squared_lm_mod, newdata = validation_split[, !"SalePrice", with = F])

# All failed predictions set to zero
chi_squared_lm_mod_pred[is.na(chi_squared_lm_mod_pred)] <- 0

# Combine predicted and observed SalePrice from validation data 
my_data_chi = as.data.frame(cbind(predicted = chi_squared_lm_mod_pred, observed = validation_split$SalePrice))

# Save RMSE
chi_squared_lm_mod_RMSE <- sqrt(mean((chi_squared_lm_mod_pred - validation_split$SalePrice)^2))

# Plot Actual vs Predicted values
ggplot(data = my_data_chi, aes(predicted, observed)) + geom_point() + geom_smooth(method = "lm") + labs(x="Predicted") +
  ggtitle('Linear Model')

# Print Results
paste("Chi-Squared Filtered Linear Regression RMSE = ", chi_squared_lm_mod_RMSE)


# ---- Information Gain Selection ----
### Calculate information gain on training data
info_gain_weights <- data.frame(information.gain(SalePrice~., dt_train))

# Display variables ranked by importance
info_gain_weights$feature <- rownames(info_gain_weights)
info_gain_weights[order(info_gain_weights$attr_importance, decreasing = TRUE),][1]

# Set importance threshold
info_gain_thres <- 0.27

# Subset name features thats pass threshold
info_gain_features <- info_gain_weights$feature[info_gain_weights$attr_importance >= info_gain_thres]

### Compute new LM with subsetted features
info_gain_lm_mod <- train(SalePrice ~ ., data = train_split[, c(info_gain_features, "SalePrice"), with = F], 
                          method = "lm", 
                          metric = "RMSE",
                          preProc = c("center", "scale"),
                          trControl = train_control_config_filter)

# Run model over each variable in validation data
for (x in names(validation_split)) {
  info_gain_lm_mod$xlevels[[x]] <- union(info_gain_lm_mod$xlevels[[x]], levels(validation_split[[x]]))
}
# Make predictions for SalePrice in validation data
info_gain_lm_mod_pred <- predict(info_gain_lm_mod, newdata = validation_split[, !"SalePrice", with = F])

# All failed predictions set to zero
info_gain_lm_mod_pred[is.na(info_gain_lm_mod_pred)] <- 0

# Combine predicted and observed SalePrice from validation data 
my_data_info_gain = as.data.frame(cbind(predicted = info_gain_lm_mod_pred, observed = validation_split$SalePrice))

# Save RMSE
info_gain_lm_mod_RMSE <- sqrt(mean((info_gain_lm_mod_pred - validation_split$SalePrice)^2))

# Plot Actual vs Predicted values
ggplot(data = my_data_info_gain, aes(predicted, observed)) + geom_point() + geom_smooth(method = "lm") + labs(x="Predicted") +
  ggtitle('Linear Model')

# Print Results
paste("IG Filtered Linear Regression RMSE = ", info_gain_lm_mod_RMSE)




