# Bitcoin 
# Principle Component Regression Model
#------------------------------------------------------------------------------
pca.data <- data[,2:6]
pca <- princomp(pca.data, cor=T)
summary(pca, loadings=T)

# Generalised Linear Model using PCA
y <- data[,1]
yr <- as.matrix(y - mean(y)) # mean centering btc.price
model <- glm(y ~ pca$scores[,1] + pca$scores[,2] + pca$scores[,3], family=gaussian)
summary(model)

plot(pca$scores[,1])
barplot(pca$scores[,1])

data$pca.pred <- predict(model) # predicted price
# plot of pca vs price
j <- as.data.frame(data) # convert to dataframe -> shouldn't used xts file :/
plot_ly(j, x = index(data), y = j$pca.pred , name = 'pca', type = 'scatter', mode = 'lines') %>%
  add_trace(x = index(data), y = j$btc.price, name = 'log(btc.price)', mode = 'lines')

#----------------------------------------------------------
# Principle Component Analysis -> solution to multicollinearity (using pcr)
train <- data["2014-01-01/2017-01-01", 1:6]
y_test <- data["2017-01-01/", 1]
test <- data["2017-01-01/", 1:6]

pcr_model <- pcr(btc.price~., data = train, scale = TRUE, validation = "CV")
summary(pcr_model)

# Plot the root mean squared error
validationplot(pcr_model)
# Plot the cross validation MSE
validationplot(pcr_model, val.type="MSEP")
# Plot the R2
validationplot(pcr_model, val.type = "R2")
predplot(pcr_model)
plot(pcr_model)

test$prediction <- predict(pcr_model, test, ncomp = 3)
mean((pcr_pred - y_test)^2)

pcr_model <- pcr(btc.price~., data = train, scale = TRUE, validation = "CV")

test$pcr.pred <- predict(pcr_model, test, ncomp = 2)
mean((test$pcr.pred - y_test)^2)