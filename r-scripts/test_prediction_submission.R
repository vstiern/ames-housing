# ---- Prediction on Test Data ----
# Prediction on Test Data with chosen model (in log)
lasso_test_prediction <- predict(lasso_mod, s = lasso_best_lam, data.matrix(dt_test[, !"Id", with = F]))

# Analysis of predictions
summary(lasso_test_prediction)
summary(info_gain_lm_mod_pred)
summary(lasso_pred_best_lam)

# Actual Sales Price (de-log with exp)
test_pred <- exp(lasso_test_prediction)-1
validation_pred <- exp(lasso_pred_best_lam)-1
train_values <- exp(dt_train$SalePrice)-1

# Analysis of predictions
summary(test_pred)
summary(validation_pred)
summary(train_values)
# Plot histogram of predictions

# Plot Distrubution
hist(test_pred)
hist(validation_pred)
hist(train_values)

View(test_pred)
#hist(exp(dt_train$SalePrice)-1)

# ---- Submission -----
# Save ID and SalePrice 
submit <- data.frame(Id = dt_test$Id, SalePrice = test_pred)
#  Name columns
colnames(submit) <-c("Id", "SalePrice")

# Set all failed predictions (NAs) to zero
submit$SalePrice[is.na(submit$SalePrice)] <- 0
# Calculate replacement value for NAs now set to zero -> sum of saleprice / (nr of rows - nr of NAs)
replace_value_for_na <- sum(na.omit(submit$SalePrice))/(nrow(submit) - sum(submit$SalePrice == 0))
submit$SalePrice[submit$SalePrice == 0] <- replace_value_for_na

summary(submit$SalePrice)

# Write to file
write.csv(submit,file="lasso_final.csv", row.names=F)

