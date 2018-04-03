### Data Transformation 

# ---- Log transformation ----

### Log transformations - Training and Test Data
# Create list with skewness for all numeric variables for test_data
list_skewness <- dt_train[, sapply(.SD, skewness, na.rm = T), .SDcols = names(which(sapply(dt_train, is.integer)))]
list_skewness <- c(list_skewness, dt_train[, sapply(.SD, skewness, na.rm = T), .SDcols = names(which(sapply(dt_train, is.numeric)))])

# Create list with skewness for all numeric variables for test_data -> absolute
#list_skewness <- abs(dt_train[, sapply(.SD, skewness, na.rm = T), .SDcols = !names(which(sapply(dt_train, is.factor)))])
#list_skewness_test <- abs(dt_test[, sapply(.SD, skewness, na.rm = T), .SDcols = !names(which(sapply(dt_test, is.factor)))])

# Exclude polynomials
ploy_columns <- c(names(list_skewness)[names(list_skewness) %like% "s2"],
                  names(list_skewness)[names(list_skewness) %like% "s3"],
                  names(list_skewness)[names(list_skewness) %like% "sqrt"])

list_skewness <- list_skewness[!names(list_skewness) %in% ploy_columns]

# Set threshold for skewness
skew_thres <- 0.65

# Subset all columns based on skewness threshold  
skewed_columns <- unique(names(list_skewness[list_skewness > skew_thres]))
skewed_columns_test <- unique(skewed_columns[skewed_columns != "SalePrice"])
skewed_columns
skewed_columns_test

# Transform skewed columns using log1p
dt_train[, (skewed_columns) := lapply(.SD, log1p), .SDcols = skewed_columns] 
dt_test[, (skewed_columns_test) := lapply(.SD, log1p), .SDcols = skewed_columns_test] 
