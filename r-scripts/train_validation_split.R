# ---- Train, Validation Spliting ----
# Split data into training and validation
### Function for splitting
splitdt <- function(DT, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(DT)
  trainindex <- sample(index, trunc(length(index) / 1.3))
  trainset <- DT[trainindex, ]
  testset <- DT[-trainindex, ]
  list(trainset = trainset, testset = testset)
}

### Execute spliting 
splits <- splitdt(dt_train, seed = seed_setting)
train_split <- splits$trainset
validation_split <- splits$testset

# ---- Remove Low Level Factor Variables ----
# Training Split
### Get Factor variables from training split
train_factor_columns <- names(which(sapply(train_split, is.factor)))
### Show number of factor levels per variable
sort(sapply(lapply(na.omit(train_split[, train_factor_columns, with = F]), droplevels), nlevels), decreasing = F)

# Validation Split
### Get Factor variables from training split
validation_factor_columns <- names(which(sapply(validation_split, is.factor)))
### Show number of factor levels per variable
sort(sapply(lapply(na.omit(validation_split[, validation_factor_columns, with = F]), droplevels), nlevels), decreasing = F)

# Test Data
### Get Factor variables from training split
test_factor_columns <- names(which(sapply(dt_test, is.factor)))
### Show number of factor levels per variable
sort(sapply(lapply(na.omit(dt_test[, test_factor_columns, with = F]), droplevels), nlevels), decreasing = F)

