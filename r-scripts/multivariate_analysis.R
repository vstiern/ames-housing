### Data Analysis ----


# ---- Univariate / Multivariate Analysis ----
### Shiny Apps goes here

### SalePrice
# Histogram
ggplot(data=dt_train, aes(SalePrice)) + geom_histogram()
# -- SalePrice is skewed -> try for log of SalePrice

# DT with normal SalePrice and log(SalePrice)
dt_hist_saleprice <- rbind(data.table(version="log(SalePrice+1)", x=log(dt_train$SalePrice + 1)),
                           data.table(version="SalePrice", x = dt_train$SalePrice))

# Histogram of log + normal
ggplot(data = dt_hist_saleprice) + facet_wrap(~version,ncol=2,scales="free_x") +
  geom_histogram(aes(x=x), bins = 50)
# -- Log(SalePrice) looks normally distrubuted, skewness and kurtosis decreased

### SaleCondition (Nominal): Condition of sale
summary(dt_train$SaleCondition)
#Plot vs SalePrice
ggplot(data=dt_train, aes(x = SaleCondition, y = SalePrice)) + geom_boxplot(aes(fill = factor(SaleCondition))) +
  theme(axis.text.x = element_text(angle = 0, size = 10), legend.position = "none")
#Plot vs GrLivArea
ggplot(data=dt_train, aes(x = SaleCondition, y = GrLivArea)) + geom_boxplot(aes(fill = factor(SaleCondition))) +
  theme(axis.text.x = element_text(angle = 0, size = 10), legend.position = "none")
# -- Abnormal, adjland, alloca, family sales below normal price. Partial above. -> Only use Normal sales?

### SalePrice vs GrLivArea
ggplot(dt_train, aes(x=GrLivArea, y=SalePrice)) + geom_point() +  geom_smooth(method=lm)
# -- Two outliers, very high area compared low price -> Perhaps remove Outliers in outlier section.

### SalePrice vs Lotfrontage 
ggplot(dt_train, aes(x=LotFrontage, y=SalePrice)) + geom_point() +  geom_smooth(method=lm)
# --Two outliers, very high area compared low price -> Perhaps remove Outliers in outlier section.

### SalePrice vs BsmtCond 
ggplot(dt_train, aes(x=BsmtCond, y=SalePrice)) + geom_point() + geom_smooth(method=lm)


# ---- Correlation ----
### Update numeric variable names
numeric_columns <- c(names(which(sapply(dt_train, is.integer))), names(which(sapply(dt_train, is.numeric))))
numeric_columns <- sort(numeric_columns[duplicated(numeric_columns) == F])

# Correlogram -> cleaned orignal data
corrplot(cor(dt_train[, numeric_columns, with = F]), method = "color", type = "lower", tl.col = "black", tl.srt = 45, order = "alphabet",
         addCoef.col = "grey", number.cex = 7 / ncol(dt_train[, numeric_columns, with = F]))

# Correlogram -> poly created
corrplot(cor(dt_train[, c(ploy_columns, "SalePrice"), with = F]), method = "color", type = "lower",
         tl.col = "black", tl.srt = 45, order = "alphabet")




### Shiny App for Correlogram
shinyApp(

  options=list(width="100%", height = 700),

  ui <- shinyUI(
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          radioButtons("option", label = "Select Addition:",
                       choices = c("None", "Significance Level" = "sig", "Correlation Coefficients" = "coeff"), selected = "None"),

          checkboxGroupInput("vars", label = "Select Variables:", choices = numeric_columns,
                             selected = c("SalePrice", "OverallCond", "GrLivArea"))
        ),
        mainPanel(
          titlePanel("Correlogram"),
          plotOutput("correplot", width = "100%", height = 700)
        )
      )
    )
  ),

  server <- shinyServer(function(input,output) {

    output$correplot = renderPlot({

      dt_sel <- dt_train[, input$vars, with = F]
      dt_cor_mat <- rcorr(as.matrix(dt_sel))

      #With signifigance test
      if(input$option == "sig") {
        corrplot(cor(dt_sel), p.mat = dt_cor_mat$P, method = "color", type = "lower", tl.col = "black",
                 tl.srt = 45, order = "alphabet", sig.level = c(.001, .01, .05), pch.cex = 10/ncol(dt_sel), insig = "label_sig",
                 pch.col = "grey")
      }
      #With correlations coeffecients
      if(input$option == "coeff") {
        corrplot(cor(dt_sel), method = "color", type = "lower", tl.col = "black", tl.srt = 45, order = "alphabet",
                 addCoef.col = "grey", number.cex = 7/ncol(dt_sel))
      }
      #With no additions
      if(input$option == "None")
        corrplot(cor(dt_sel), method = "color", type = "lower", tl.col = "black", tl.srt = 45, order = "alphabet")
    })
  })
)


