### Modeling: Wrapper Methods
# ---- Backward Stepwise ----

# Define configurations
train_control_config_4_stepwise <- trainControl(method = "none")

# Compute new LM using backward stepwise
backward_lm_mod <- train(SalePrice ~ ., data = train_split, 
                         method = "glmStepAIC", 
                         direction = "backward",
                         trace = FALSE,
                         metric = "RMSE",
                         steps = 10,
                         preProc = c("center", "scale"),
                         trControl = train_control_config_4_stepwise)

# Features chosen by model
paste("Features Selected" , backward_lm_mod$finalModel$formula[3])

# Uniate levels from model with validation split
for (x in names(validation_split)) {
  backward_lm_mod$xlevels[[x]] <- union(backward_lm_mod$xlevels[[x]], levels(validation_split[[x]]))
}
# Make predictions for SalePrice in validation data
backward_lm_mod_pred <- predict(backward_lm_mod, validation_split[, !"SalePrice", with = F])

# All failed predictions set to zero
backward_lm_mod_pred[is.na(backward_lm_mod_pred)] <- 0

# Combine predicted and observed SalePrice from validation data 
my_data_back = as.data.frame(cbind(predicted = backward_lm_mod_pred, observed = validation_split$SalePrice))

# Save RMSE
backward_lm_mod_RMSE <- sqrt(mean((backward_lm_mod_pred - validation_split$SalePrice)^2))

# Plot Actual vs Predicted values
ggplot(data = my_data_back, aes(predicted, observed)) + geom_point() + geom_smooth(method = "lm") + labs(x="Predicted") +
  ggtitle('Linear Model')

# Print Results
paste("Backward Linear Regression RMSE = ", backward_lm_mod_RMSE)

# ---- Forward Stepwise ----

# Define configurations
train_control_config_4_stepwise <- trainControl(method = "none")

# Compute new LM using forward stepwise
forward_lm_mod <- train(x = train_split[, !"SalePrice", with = F], y = train_split$SalePrice, 
                        method = "glmStepAIC", 
                        direction = "forward",
                        trace = FALSE,
                        metric = "RMSE",
                        steps = 15,
                        preProc = c("center", "scale"),
                        trControl = train_control_config_4_stepwise)

# Features chosen by model
paste("Features Selected" , forward_lm_mod$finalModel$formula[3])

# Uniate levels from model with validation split
for (x in names(validation_split)) {
  forward_lm_mod$xlevels[[x]] <- union(forward_lm_mod$xlevels[[x]], levels(validation_split[[x]]))
}

# Make predictions for SalePrice in validation data
forward_lm_mod_pred <- predict(forward_lm_mod, validation_split[, !"SalePrice", with = F])

# All failed predictions set to zero
forward_lm_mod_pred[is.na(forward_lm_mod_pred)] <- 0

# Combine predicted and observed SalePrice from validation data 
my_data_forward = as.data.frame(cbind(predicted = info_gain_lm_mod_pred, observed = validation_split$SalePrice))

# Plot Actual vs Predicted values
ggplot(data = my_data_forward, aes(predicted, observed)) + geom_point() + geom_smooth(method = "lm") + labs(x="Predicted") +
  ggtitle('Linear Model')

# Save RMSE
forward_lm_mod_RMSE <- sqrt(mean((forward_lm_mod_pred - validation_split$SalePrice)^2))

# Print results
paste("Forward Linear Regression RMSE = ", forward_lm_mod_RMSE)

