# Assess Multicollinearity
linear_combinations <- findLinearCombos(data.matrix(dt_test))
colnames(dt_test)[linear_combinations$linearCombos[[1]]]
colnames(dt_test)[linear_combinations$linearCombos[[2]]]
colnames(dt_test)[linear_combinations$remove]
# correlation due to house size. -> no need to explain what predict house price -> thus no need to drop columns
