### Data Ingestion & Cleaning ----

### Data Ingestion
dt_train <- data.table(read.csv("train.csv"))
dt_test <- data.table(read.csv("test.csv"))

# ---- Initial Data Inspection ----
### Columns same for Test and Train data
colnames(dt_train)
colnames(dt_test) # test data missing SalePrice as assumed
length(dt_train$Id) == nrow(dt_train) # all rows unique Id thuis no duplicates and Id can be dropped

### Drop Columns
dt_train$Id <- NULL

# Merge test and training data
dt_All <- rbind(dt_train[, !"SalePrice"], dt_test[, !"Id"])

# ---- Variable Identification ----
table(sapply(dt_train, class))
table(sapply(dt_test, class))

# ---- Refactorize Known Features ----
### Change class of variables
# MSSubClass: The building class -> not numerical -> factor
dt_train[, MSSubClass := as.factor(MSSubClass)]
dt_test[, MSSubClass := as.factor(MSSubClass)]

# MoSold: Month Sold -> not numerical -> factor
dt_train[, MoSold := as.factor(MoSold)]
dt_test[, MoSold := as.factor(MoSold)]

# --- Factor Level Anlysis ----
### Get Factor variables from training split
train_factor_columns <- names(which(sapply(dt_train, is.factor)))
test_factor_columns <- names(which(sapply(dt_test, is.factor)))
### Show number of factor levels per variable
sort(sapply(lapply(dt_train[, train_factor_columns, with = F], droplevels), nlevels), decreasing = F)
sort(sapply(lapply(dt_test[, test_factor_columns, with = F], droplevels), nlevels), decreasing = F)

### Utilites -> remove all properties that doesn't have all public utilities -> only one reaming -> drop feature
dt_train$Utilities <- NULL
dt_test$Utilities <- NULL

# --- Inspection of Noteworthy Variables ----
### MSZoning -> C (meaning commercial) properties?
summary(dt_train$MSZoning) # 10 commerical properties
summary(dt_test$MSZoning) # 15 commerical properties
# Plot relationship with SalePrice
ggplot(dt_train, aes(x = reorder(MSZoning, SalePrice, FUN = mean), y = SalePrice)) + geom_boxplot() 
# Mean price by type
dt_train[, mean(SalePrice), by = MSZoning]
# Remove or keep? -> Keep, as it might be good regressor for test data. 


# ---- Missing Values Treatment ----
# Count NAs per colums and subset for columns with NAs > 0
list_NAs <- sapply(dt_train, function(x) sum(is.na(x)))
list_NAs <- sort(list_NAs[list_NAs > 0], decreasing = T)
list_NAs
paste("Number of variables in training data that contains NA: ", ifelse(length(list_NAs)>0, length(list_NAs), 0))

# Save column names
NA_columns <- names(list_NAs)

# Check NAs for test data
list_NAs_test <- sapply(dt_test, function(x) sum(is.na(x)))
list_NAs_test <- sort(list_NAs_test[list_NAs_test > 0], decreasing = T)
list_NAs_test
paste("Number of variables in test data that contains NA: ", ifelse(length(list_NAs_test)>0, length(list_NAs_test), 0))

### Treat NAs per variable
# LotFrontace (Linear feet of street connected to property)
summary(dt_train$LotFrontage) # -> Predict values based on LotArea, LotShape and neighbourhood

# LotArea: Plot relationship -> Relationship analysis reveal log is optimal.
ggplot(dt_train, aes(x=log(LotArea), y=log(LotFrontage))) + geom_point() + geom_smooth(method=lm)

# LotShape -> Plot relationship -> IR3 higher mean
ggplot(dt_train, aes(x = LotShape, y = LotFrontage)) + geom_boxplot()
summary(dt_train$LotShape)
summary(dt_test$LotShape)

# LotConfig -> Plot relationship -> CulDSac lower mean
ggplot(dt_train, aes(x = LotConfig, y = LotFrontage)) + geom_boxplot()
summary(dt_train$LotConfig)
summary(dt_test$LotConfig)

# Neighbourhood: -> Mean varies across different hoods -> strong enough to use?
ggplot(dt_train, aes(x = LotConfig, y = LotFrontage)) + geom_boxplot()
dt_train[, median(LotFrontage, na.rm = T), by = Neighborhood]

# Build model
LotFrontage_lm <- lm(data = dt_All, log(LotFrontage) ~ log(LotArea) + LotShape + LotConfig)

# Predict NAs using model -> LotFrontage is integer
predict_LotFrontage_train <- as.integer(exp(predict(LotFrontage_lm, newdata = dt_train[is.na(LotFrontage), ])))
predict_LotFrontage_test <- as.integer(exp(predict(LotFrontage_lm, newdata = dt_test[is.na(LotFrontage), ])))

# See distrubution of prediction for traning and test
hist(predict_LotFrontage_train, breaks = 50)
hist(predict_LotFrontage_test, breaks = 50)

# Impute NAs for Train and Test data
dt_train[is.na(LotFrontage), LotFrontage := predict_LotFrontage_train]
dt_test[is.na(LotFrontage), LotFrontage := predict_LotFrontage_test]

### Alley (alley access)
summary(dt_train$Alley)
# -- Notes states NA = No alley access. 
dt_train[is.na(Alley), Alley := "None"]
dt_test[is.na(Alley), Alley := "None"]

### MasVnrType (Masonry veneer type)
summary(dt_train$MasVnrType)
# -- 4 levels: BrkCmn / BrkFace / Stone / None -> NA probably means None
dt_train[is.na(MasVnrType), MasVnrType := "None"]
dt_test[is.na(MasVnrType), MasVnrType := "None"]

### MasVnrArea (Masonry veneer area in square feet)
summary(dt_train$MasVnrArea)
# -- If MasVnrType = None, MasVnrArea NAs = 0
dt_train[is.na(MasVnrArea), MasVnrArea := 0]
dt_test[is.na(MasVnrArea), MasVnrArea := 0]

### BsmtQual (Evaluates the height of the basement)
summary(dt_train$BsmtQual)
# -- Notes states NA = No Basement
dt_train[is.na(BsmtQual), BsmtQual := "None"]
dt_test[is.na(BsmtQual), BsmtQual := "None"]

### BsmtCond (Evaluates the general condition of the basement)
summary(dt_train$BsmtCond)
# -- Notes states NA = No Basement
dt_train[is.na(BsmtCond), BsmtCond := "None"]
dt_test[is.na(BsmtCond), BsmtCond := "None"]

### BsmtExposure (Refers to walkout or garden level walls)
summary(dt_train$BsmtExposure)
# -- Notes states NA = No Basement
dt_train[is.na(BsmtExposure), BsmtExposure := "None"]
dt_test[is.na(BsmtExposure), BsmtExposure := "None"]

### BsmtFinType1 (Rating of basement finished area)
summary(dt_train$BsmtFinSF1)
# -- Notes states NA = No Basement
dt_train[is.na(BsmtFinType1), BsmtFinType1 := "None"]
dt_test[is.na(BsmtFinType1), BsmtFinType1 := "None"]

### BsmtFinType2 (Rating of basement finished area (if multiple types))
summary(dt_train$BsmtFinSF2)
# -- Notes states NA = No Basement
dt_train[is.na(BsmtFinType2), BsmtFinType2 := "None"]
dt_test[is.na(BsmtFinType2), BsmtFinType2 := "None"]

### Electrical (Electrical system)
summary(dt_train$Electrical)
# -- 1 NA, categorical mode imputation
dt_train[is.na(Electrical), Electrical := "SBrkr"]
dt_test[is.na(Electrical), Electrical := "SBrkr"]

### FireplaceQu (Fireplace quality)
summary(dt_train$FireplaceQu)
# -- Notes states NA = No fireplace
dt_train[is.na(FireplaceQu), FireplaceQu := "None"]
dt_test[is.na(FireplaceQu), FireplaceQu := "None"]

### GarageType (Garage location)
summary(dt_train$GarageType)
# -- Notes states NA = No Garage
dt_train[is.na(GarageType), GarageType := "None"]
dt_test[is.na(GarageType), GarageType := "None"]

### GarageYrBlt (Year garage was built)
summary(dt_train$GarageYrBlt)
# -- NA probably means no garage, however we assume garage was built same year as house
dt_train[is.na(GarageYrBlt), GarageYrBlt := YearBuilt]
dt_test[is.na(GarageYrBlt), GarageYrBlt := YearBuilt]

### GarageFinish (Interior finish of the garage)
summary(dt_train$GarageFinish)
# -- Notes states NA = No Garage
dt_train[is.na(GarageFinish), GarageFinish := "None"]
dt_test[is.na(GarageFinish), GarageFinish := "None"]

### GarageQual (Garage quality)
summary(dt_train$GarageQual)
# -- Notes states NA = No Garage
dt_train[is.na(GarageQual), GarageQual := "None"]
dt_test[is.na(GarageQual), GarageQual := "None"]

### GarageCond (Garage quality)
summary(dt_train$GarageCond)
# -- Notes states NA = No Garage
dt_train[is.na(GarageCond), GarageCond := "None"]
dt_test[is.na(GarageCond), GarageCond := "None"]

### PoolQC (Pool quality)
summary(dt_train$PoolQC)
# -- Notes states NA = No Pool. Majoirty NAs -> drop variable?
dt_train[is.na(PoolQC), PoolQC := "None"]
dt_test[is.na(PoolQC), PoolQC := "None"]

### Fence (Fence quality)
summary(dt_train$Fence)
# -- Notes states NA = No Fence. Majoirty NAs -> drop variable?
dt_train[is.na(Fence), Fence := "None"]
dt_test[is.na(Fence), Fence := "None"]

### MiscFeature (Miscellaneous feature not covered in other categories)
summary(dt_train$MiscFeature)
# -- Notes states NA = None extra comments. Majoirty NAs -> drop variable?
dt_train[is.na(MiscFeature), MiscFeature := "None"]
dt_test[is.na(MiscFeature), MiscFeature := "None"]


### Re-check training data NA columns
list_NAs <- sapply(dt_train, function(x) sum(is.na(x)))
list_NAs <- sort(list_NAs[list_NAs > 0], decreasing = T)
list_NAs
paste("Number of variables in training data that contains NA: ", ifelse(length(list_NAs)>0, length(list_NAs), 0))

# ---- Unique Missing Values Treatment for Test Data ----
list_NAs_test <- sapply(dt_test, function(x) sum(is.na(x)))
list_NAs_test <- sort(list_NAs_test[list_NAs_test > 0], decreasing = T)
list_NAs_test
paste("Number of variables in test data that contains NA: ", ifelse(length(list_NAs_test)>0, length(list_NAs_test), 0))

### MSZoning
summary(dt_test$MSZoning)
ggplot(dt_train, aes(x = reorder(MSZoning, SalePrice, FUN = mean), y = SalePrice)) + geom_boxplot() 

# Find Characteristics of MSZoning Commerical -> Pattern in Neighborhood (IDOTRR), GrLivArea (Relatively Smaller)
dt_test[MSZoning == "C (all)", ]

# GrLivArea (Relatively Smaller) -> by median yes!
dt_test[, median(GrLivArea), by = MSZoning]

# Neighborhood (IDOTRR) -> 13/53 Commerical for IDOTRR, but it hold 13/15 of all commercial
table(dt_test[, Neighborhood, by = MSZoning])

# MSZoning NAs -> IDOTRR with low GrLivArea -> Commerical property
dt_test[is.na(MSZoning), c("MSZoning", "Neighborhood", "GrLivArea")]

# Impute NAs 
dt_test[is.na(MSZoning) & Neighborhood == "IDOTRR"  & GrLivArea < 1000, MSZoning := "C (all)"] # Commerical properties
dt_test[is.na(MSZoning) & Neighborhood == "IDOTRR", MSZoning := "RM"] # mode for specific Neighborhood
dt_test[is.na(MSZoning) & Neighborhood == "Mitchel", MSZoning := "RL"] # mode for specific Neighborhood

### BasmtFullBath
summary(dt_test$BsmtFullBath)
dt_test[is.na(BsmtFullBath), ]
# NA due to no basement -> Impute 0
dt_test[is.na(BsmtFullBath), BsmtFullBath := 0]

### BsmtHalfBath
summary(dt_test$BsmtHalfBath)
dt_test[is.na(BsmtHalfBath), ]
# NA due to no basement -> Impute 0
dt_test[is.na(BsmtHalfBath), BsmtHalfBath := 0]

### Exterior1st
summary(dt_test$Exterior1st)
ggplot(dt_train, aes(x = reorder(Exterior1st, SalePrice, FUN = mean), y = SalePrice)) + geom_boxplot() 
# Find characteristics of prpotery
dt_test[is.na(Exterior1st),]
dt_test[is.na(Exterior1st), c("Neighborhood", "MSSubClass", "BldgType", "HouseStyle", "ExterQual", "YearRemodAdd")]
# Look for relationship between descriptive parameters and features
table(dt_test[, Exterior1st, by = MSSubClass])
table(dt_test[, Exterior1st, by = HouseStyle])
table(dt_test[, Exterior1st, by = ExterQual])
table(dt_test[, Exterior1st, by = YearRemodAdd]) # might suggest Vinyl if exterior was remade...
table(dt_test[Neighborhood == "Edwards", Exterior1st, by = MSSubClass])
table(dt_test[Neighborhood == "Edwards", Exterior1st, by = HouseStyle])
table(dt_test[Neighborhood == "Edwards", Exterior1st, by = ExterQual])
table(dt_test[Neighborhood == "Edwards", Exterior1st, by = YearRemodAdd]) # might suggest Vinyl if exterior was remade...

# Impute NAs with categorcial mode value for Neighborhood and exterier quality. 
dt_test[is.na(Exterior1st), Exterior1st := "Wd Sdng"] 

### Exterior2nd -> Assume building only in one type of material
summary(dt_test$Exterior2nd)
# How likely is it for 2nd material?
table(dt_test[, Exterior1st, by = Exterior2nd])
# # How likely is it for 2nd material given Neighborhood and Exterior1st = wd sdng
table(dt_test[Exterior1st == "Wd Sdng" & Neighborhood == "Edwards", Exterior2nd])

# Impute NAs with same value as Exterior1st
dt_test[is.na(Exterior2nd), Exterior2nd := "Wd Sdng"] 

### BsmtFinSF1
summary(dt_test$BsmtFinSF1)
dt_test[is.na(BsmtFinSF1), ]
# NA due to no basement -> Impute 0
dt_test[is.na(BsmtFinSF1), BsmtFinSF1 := 0]

### BsmtFinSF2
summary(dt_test$BsmtFinSF2)
dt_test[is.na(BsmtFinSF2), ]
# NA due to no basement -> Impute 0
dt_test[is.na(BsmtFinSF2), BsmtFinSF2 := 0]

### BsmtUnfSF
summary(dt_test$BsmtUnfSF)
dt_test[is.na(BsmtUnfSF), ]
# NA due to no basement -> Impute 0
dt_test[is.na(BsmtUnfSF), BsmtUnfSF := 0]

### TotalBsmtSF
summary(dt_test$TotalBsmtSF)
dt_test[is.na(TotalBsmtSF), ]
# NA due to no basement -> Impute 0
dt_test[is.na(TotalBsmtSF), TotalBsmtSF := 0]

### KitchenQual
summary(dt_test$KitchenQual)
dt_test[is.na(KitchenQual), ] # NA propterty remade in 1950, Neighborhood ClearCr.
# Look for average kitchen quality for properties remade between 1940-1960, 
table(dt_test[YearRemodAdd > 1940 & YearRemodAdd < 1960, KitchenQual])
# Avreage kitchen quality for neighborhood
table(dt_test[Neighborhood == "ClearCr", KitchenQual])
# Impute NAs with categorcial mode value  
dt_test[is.na(KitchenQual), KitchenQual := "TA"]

### Functional
summary(dt_test$Functional)
summary(dt_train$Functional)
ggplot(dt_train, aes(x = reorder(Functional, SalePrice, FUN = mean), y = SalePrice)) + geom_boxplot() 
dt_train[, mean(SalePrice), by = Functional]
# Impute NAs with categorcial mode value  
dt_test[is.na(Functional), Functional := "Typ"]

### GarageCars
summary(dt_test$GarageCars)
dt_test[is.na(GarageCars),]
# -- Notes states NA = No Garage -> 0 spots
dt_test[is.na(GarageCars), GarageCars := 0]

### GarageArea
summary(dt_test$GarageArea)
dt_test[is.na(GarageArea),]
# -- Notes states NA = No Garage -> 0 spots
dt_test[is.na(GarageArea), GarageArea := 0]

### SaleType
summary(dt_test$SaleType)
ggplot(dt_train, aes(x = reorder(SaleType, SalePrice, FUN = mean), y = SalePrice)) + geom_boxplot() 
# Features for NA properties
dt_test[is.na(SaleType),] # SaleCondition Normal
table(dt_test[SaleCondition == "Normal", SaleType])

# Impute NAs with categorcial mode value  
dt_test[is.na(SaleType), SaleType := "WD"]

### Recheck NAs for test data
list_NAs_test <- sapply(dt_test, function(x) sum(is.na(x)))
list_NAs_test <- sort(list_NAs_test[list_NAs_test > 0], decreasing = T)
list_NAs_test
paste("Number of variables in test data that contains NA: ", ifelse(length(list_NAs_test)>0, length(list_NAs_test), 0))
