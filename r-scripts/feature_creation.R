# ---- Feature Creation

# ---- Ordinal Feature Creation  ----
# Function Transform ordinal variables to numeric scale
ordinal_transformation <- function(dt) {
  ### Basements
  dt[, OrdinalBsmtQual := dplyr::recode(BsmtQual, "None" = 0, "Po" = 1, "Fa" = 2, "TA" = 3, "Gd" = 4, "Ex" = 5)]
  dt[, OrdinalBsmtCond := dplyr::recode(BsmtCond, "None" = 0, "Po" = 1, "Fa" = 2, "TA" = 3, "Gd" = 4, "Ex" = 5)]
  dt[, OrdinalBsmtExposure := dplyr::recode(BsmtExposure, "None" = 0, "No" = 1, "Mn" = 2, "Av" = 3, "Gd" = 4)]
  dt[, OrdinalBsmtFinType1 := dplyr::recode(BsmtFinType1, "None" = 0, "Unf" = 1, "LwQ" = 2, "Rec" = 3, "BLQ" = 4, "ALQ" = 5, "GLQ" = 6)]
  dt[, OrdinalBsmtFinType2 := dplyr::recode(BsmtFinType2, "None" = 0, "Unf" = 1, "LwQ" = 2, "Rec" = 3, "BLQ" = 4, "ALQ" = 5, "GLQ" = 6)]
  ### Garage
  dt[, OrdinalGarageFinish := dplyr::recode(GarageFinish, "None" = 1, "Unf" = 1, "RFn" = 2, "Fin" = 3)]
  dt[, OrdinalGarageQual := dplyr::recode(GarageQual, "None" = 0, "Po" = 1, "Fa" = 2, "TA" = 3, "Gd" = 4, "Ex" = 5)]
  dt[, OrdinalGarageCond := dplyr::recode(GarageCond, "None" = 0, "Po" = 1, "Fa" = 2, "TA" = 3, "Gd" = 4, "Ex" = 5)]
  ### Kitchen
  dt[, OrdinalKitchenQual := dplyr::recode(KitchenQual, "None" = 0, "Po" = 1, "Fa" = 2, "TA" = 3, "Gd" = 4, "Ex" = 5)]
  ### Utilities
  dt[, OrdinalHeatingQC := dplyr::recode(HeatingQC, "None" = 0, "Po" = 1, "Fa" = 2, "TA" = 3, "Gd" = 4, "Ex" = 5)]
  dt[, OrdinalElectrical := dplyr::recode(Electrical, "None" = 0, "Unkown" = 1, "Mix" = 2, "FuseP" = 1, "FuseF" = 2, "FuseA" = 3, "SBrkr" = 4)]
  ### Exterior
  dt[, OrdinalExterCond := dplyr::recode(ExterCond, "None" = 0, "Po" = 1, "Fa" = 2, "TA"= 3, "Gd"= 4, "Ex" = 5)]
  dt[, OrdinalExterQual := dplyr::recode(ExterQual, "None" = 0, "Po" = 1, "Fa" = 2, "TA"= 3, "Gd"= 4, "Ex" = 5)]
  ### LandFeatures
  dt[, OrdinalLandSlope := dplyr::recode(LandSlope, "None" = 0, "Sev" = 1, "Mod" = 2, "Gtl" = 3)]
  dt[, OrdinalLotShape := dplyr::recode(LotShape, "None" = 0, "IR3" = 1, "IR2" = 2, "IR1" = 3, "Reg" = 4)]
  ### Fireplaces
  dt[, OrdinalFireplaceQu := dplyr::recode(FireplaceQu, "None" = 0, "Po" = 1, "Fa" = 2, "TA"= 3, "Gd"= 4, "Ex" = 5)]
  ### Pool
  dt[, OrdinalPoolQC := dplyr::recode(PoolQC, "None" = 0, "Po" = 1, "Fa" = 2, "TA"= 3, "Gd"= 4, "Ex" = 5)]
  ### Fence
  dt[, OrdinalFence := dplyr::recode(Fence, "None" = 0, "MnWw" = 1, "GdWo" = 2, "MnPrv" = 3, "GdPrv" = 4)]
  ### Driveway
  dt[, OrdinalPavedDrive := dplyr::recode(PavedDrive, "None" = 0, "N" = 0, "P" = 1, "Y"= 2)]
  ### Street
  dt[, OrdinalStreet := dplyr::recode(Street, "None" = 0, "Grvl" = 1, "Pave" = 2)]
}

# Run function for train and test data
ordinal_transformation(dt_train)
ordinal_transformation(dt_test)

# ---- Simplefied Feature Creation ----
feature_simplification <- function(dt) {
  
  ### RoofMaterial - shrink to 2 levels -> CompShg / Other
  dt[, SimpleRoofMatl := dplyr::recode(RoofMatl, "ClyTile" = "Other", "Membran" = "Other", "Metal" = "Other", "Roll" = "Other",
                                       "Tar&Grv" = "Other", "WdShake" = "Other", "WdShngl" = "Other")]
  
  ### RoofStyle - shrink to 3 levels -> Gable / Hip / Other
  dt[, SimpleRoofStyle := dplyr::recode(RoofStyle, "Flat" = "Other", "Gambrel" = "Other", "Mansard" = "Other", "Shed" = "Other")]
  
  ### HouseStyle - shrink to 3 levels -> 1Story / 2Story / Other
  dt[, SimpleHouseStyle := dplyr::recode(HouseStyle, "1.5Fin" = "1Story", "1.5Unf" = "1Story", "2.5Fin" = "2Story", "2.5Unf" = "2Story",
                                         "SFoyer" = "Other", "SLvl" = "Other")]
  
  ### BldgType - shrink to 3 levels -> 1Fam / TwnhsE / Other
  dt[, SimpleBldgType := dplyr::recode(BldgType, "2fmCon" = "Other", "Duplex" = "Other", "Twnhs" = "Other")]
  
  ### Condition1 - shrink to 3 levels -> Normal / Positive / Negative -> make ordinal
  dt[, SimpleCondition1 := dplyr::recode(Condition1, "Feedr" = "Negative", "Artery" = "Negative", "RRAe" = "Negative", "RRNe" = "Negative",
                                       "RRNe" = "Negative", "PosN" = "Positive", "PosA" = "Positive", "RRNe" = "Positive", "RRNn" = "Positive")]
  
  ### Condition2 - shrink to 2 levels -> Normal / Other
  dt[, SimpleCondition2 := dplyr::recode(Condition2, "Feedr" = "Other", "Artery" = "Other", "RRAe" = "Other", "RRNe" = "Other",
                                         "RRNe" = "Other", "PosN" = "Other", "PosA" = "Other", "RRNe" = "Other", "RRNn" = "Other")]
  
  ### Alley - shrink to 2 levels -> Yes / None
  dt[, SimpleAlley := dplyr::recode(Alley, "Grvl" = "Yes", "Pave" = "Yes")]
  
  ### Functional - shrink to 2 levels -> Typ / Other
  dt[, SimpleFunctional := dplyr::recode(Functional, "Maj1" = "Other", "Maj2" = "Other", "Min1" = "Other", "Min2" = "Other",
                                         "Mod" = "Other", "Sev" = "Other")]
  
  ### Heating - shrink to 3 levels -> GasA / GasW / Other
  dt[, SimpleHeating := dplyr::recode(Heating, "Floor" = "Other", "Grav" = "Other", "OthW" = "Other", "Wall" = "Other")]
  
  ### MiscFeature - shrink to 2 levels -> Feature / None
  dt[, SimpleMiscFeature := dplyr::recode(MiscFeature, "Gar2" = "Feature", "Othr" = "Feature", "Shed" = "Feature", "TenC" = "Feature")]
  
  ### SaleType - shrink to 3 levels ->  WD / New / Other
  dt[, SimpleSaleType := dplyr::recode(SaleType, "COD" = "Other", "Con" = "Other", "ConLD" = "Other", "ConLI" = "Other", "ConLw" = "Other",
                                          "CWD" = "Other", "Oth" = "Other")]
  
  ### SaleCondition shrink to 2 levels ->  NotNormal / Normal
  dt[, SimpleSaleCondition := dplyr::recode(SaleCondition, "Abnorml" = "NotNormal", "Alloca" = "NotNormal", "AdjLand" = "NotNormal", "Family" = "NotNormal", "Partial" = "Normal")]
  
  ### PoolQC shrink to 2 levels ->  Pool / None
  dt[, SimplePoolQC := dplyr::recode(PoolQC, "Ex" = "Pool", "Gd" = "Pool", "Fa" = "Pool", "TA" = "Pool")]
  
  ### PavedDrive shrink to 2 levels ->  Yes / No
  dt[, SimplePavedDrive := dplyr::recode(PavedDrive, "N" = "No", "P" = "Yes", "Y" = "Yes")]
  
  ### Foundation shrink to 3 levels -> CBlock / PConc / Other
  dt[, SimpleFoundation := dplyr::recode(Foundation, "BrkTil" = "Other", "Slab" = "Other", "Stone" = "Other", "Wood" = "Other")]
  
  ### MasVnrType shrink to 2 levels ->  Yes / None
  dt[, SimpleMasVnrType := dplyr::recode(MasVnrType, "BrkCmn" = "Yes", "BrkFace" = "Yes", "Stone" = "Yes")]
  
  ### LotConfig shrink to 3 levels ->  Corner / Inside / Other
  dt[, SimpleLotConfig := dplyr::recode(LotConfig, "CulDSac" = "Other", "FR2" = "Other", "FR3" = "Other")]
  
  ### MSSubClass shrink to 3 levels ->  Newer / Older / All
  dt[, SimpleMSSubClass := dplyr::recode(MSSubClass, "20" = "New", "60" = "New", "120" = "New", "160" = "New",
                                         "30" = "Old",  "70" = "Old",
                                         "40" = "All", "45" = "All", "50" = "All", "75" = "All", "80" = "All", "85" = "All",
                                         "90" = "All", "150" = "All", "180" = "All", "190" = "All")]
  
  ### MoSold shrink to 2 levels ->  Peak / OffPeak
  dt[, SimpleMoSold := dplyr::recode(MoSold, "1" = "OffPeak", "2" = "OffPeak", "3" = "OffPeak", "4" = "OffPeak", "8" = "OffPeak",
                                     "9" = "OffPeak", "10" = "OffPeak", "11" = "OffPeak", "12" = "OffPeak",
                                     "5" = "Peak", "6" = "Peak", "7" = "Peak")]
  
  ### GarageType shrink to 5 levels ->  Attchd / Detchd / BuiltIn / Other / None
  #ggplot(dt_train, aes(x=GarageType, y=SalePrice)) + geom_boxplot()
  dt[, SimpleGarageType := dplyr::recode(GarageType, "2Types" = "Other", "Basment" = "Other", "CarPort" = "Other")]
  
}

# Run function over train and test data
feature_simplification(dt_train)
feature_simplification(dt_test)
  
# ---- New Features Creation ----

feature_creation <- function(dt) {
  
  ### House Age
  dt[, HouseAge := (YrSold - YearBuilt)]
  ### Years since remodeled
  dt[, YrsSinceRemod := (YrSold - YearRemodAdd)]
  ### Total Square Feet 
  dt[, TotalSF := (GrLivArea + TotalBsmtSF)]
  ### Total Floor Square Feet 
  dt[, TotalFloorSF := (X1stFlrSF + X2ndFlrSF)]
  ###  Average room size
  dt[, AvgRoomSize := (GrLivArea / TotRmsAbvGrd)]
  ### Comparative size of living area
  dt[, RelativeSize := (GrLivArea / mean(GrLivArea))]
  ### Total number of bathrooms
  dt[, TotNrBaths := (FullBath + (0.5 * HalfBath) + BsmtFullBath + (0.5 * BsmtHalfBath))]
  ### Bathroom to bedroom ratio: Only count above ground baths
  dt[, WCsPerBedrooms := ifelse(BedroomAbvGr == 0, 0, ((FullBath + HalfBath) / BedroomAbvGr))]
  ### Porch Size
  dt[, TotalPorchSize := (OpenPorchSF + EnclosedPorch + X3SsnPorch + ScreenPorch)]
  ### CompletedAtSale
  dt[, CompletedAtSale := ifelse(SaleCondition == "Partial", 0, 1)]
  ### NewHouse(less than 10 years hold)
  dt[, NewHouse := ifelse(YearBuilt > 2000 , 1, 0)]
  
  
  # Create aggerate score for various features
  ### Overall
  dt[, OverallScore := (OverallQual * OverallCond)]
  ### Kitchen
  dt[, KitchenScore := (OrdinalKitchenQual * KitchenAbvGr)]
  ### Basement
  dt[, BsmtScore := (OrdinalBsmtQual * OrdinalBsmtCond * (OrdinalBsmtExposure + 1) * TotalBsmtSF * OrdinalBsmtFinType1 * OrdinalBsmtFinType2)]
  ### Garage
  # GarageCars and Area strongly related, but Nr of Cars stronger.
  dt[, GarageScore := (OrdinalGarageFinish * OrdinalGarageQual * OrdinalGarageCond * (1 + GarageCars))]
  ### Utility
  dt[, UtilityScore := (OrdinalHeatingQC * OrdinalElectrical)]
  ### Exterior
  dt[, ExterScore := (OrdinalExterCond * OrdinalExterQual)]
  ### LandFeatures
  dt[, LandFeatureScore := (OrdinalLandSlope * OrdinalLotShape)]
  ### Fireplace
  dt[, FireplaceScore := (OrdinalFireplaceQu * Fireplaces)]
  ### Pool
  dt[, PoolScore := (OrdinalPoolQC * PoolArea)]
}
# Run function for train and test data
feature_creation(dt_train)
feature_creation(dt_test)

### Polynomials -> To polarize important features based on info gain and chi-squred
# Features chosen for Polynomials
poly_vars <- c("TotalSF", "OverallQual", "OrdinalHeatingQC", "GarageCars", "FullBath",
               "ExterScore", "Fireplaces", "OrdinalKitchenQual", "OrdinalBsmtQual", "OrdinalGarageFinish")

# To try
# SaleCondition (numeric?)
# OrdinalGarageFinish


# Exlcude for polynomials aka already tested
# GrLivArea
# KitchenScore
# BsmtScore
# WCsPerBedrooms

### Function to compute Polynomials 
polynomial_features <- function(dt) {

dt[, paste0(poly_vars,"-sqrt") := lapply(.SD, sqrt), .SDcols = poly_vars]
dt[, paste0(poly_vars,"-s2") := lapply(.SD, function(x) x^2), .SDcols = poly_vars]
dt[, paste0(poly_vars,"-s3") := lapply(.SD, function(x) x^3), .SDcols = poly_vars]
}
# Run function for train and test data
polynomial_features(dt_train)
polynomial_features(dt_test)
