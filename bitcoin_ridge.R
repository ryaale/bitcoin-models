# Bitcoin
# Ridge Regression Model
library(tidyverse)
library(broom)
library(glmnet)
library(lmridge)
library(MASS)
library(reshape2)
library(MASS)
# For one predictor variable -> tx.vol
# Set up data
y <- data$btc.price # price
X <- data$tx.vol # tx.vol

# least squares fit
ls.fit <- lm(y~X)

# Center the data
yr <- as.matrix(y - mean(y))
Xr <- as.matrix(X - mean(X))

# ridge regression fit
lambda <- 10*nrow(Xr)
rr.fit <- as.numeric((1/(t(Xr)%*%Xr + lambda))*t(Xr)%*%yr)

df <- data.frame(cbind(X, coef(ls.fit)[-1]*Xr, rr.fit*Xr))
colnames(df) <- c("tx.vol","ls","ridge")
df2 <- melt(df, id="tx.vol")
colnames(df2) <- c("tx.vol", "method", "price")
ggplot(data=df2, aes(x=tx.vol, y=price, colour=method)) + geom_line()

#-----------------------------------------------------------
# Ridge for more than one predictor variable
# Set up data
y <- data$btc.price # btc.price
X <- as.matrix((data[,2:6])) # all other predictors

# Center data
yr <- y - mean(y) # center y
Xr <- scale(X, center = T, scale = T)

# ridge regression fit
lambdas = exp(seq(log(.01), log(10*nrow(Xr)), l=100))
betasr = matrix(0, length(lambdas), ncol(Xr))
for(i in 1:length(lambdas))
{
  betasr[i,] = solve(t(Xr)%*%Xr + lambdas[i]*diag(ncol(Xr)))%*%t(Xr)%*%yr
}
colnames(betasr) <- names(sample.bitcoin.xts)[2:6]
df_ridge <- data.frame(cbind(lambdas, betasr, 1:length(lambdas)))
df <- melt(df_ridge, id=c("lambdas","V7"))
colnames(df) <- c("Lambda", "Lambda.Index", "Variable", "Coefficient")
ggplot(data=df, aes(x=Lambda.Index, y = Coefficient, colour = Variable)) + geom_line()
sort(abs(df_ridge[,2:6][1,]), decreasing=TRUE)
#  exch.vol tot.wallets    tx.vol tot.hashrate tot.unique.addr
# 1.095728   0.6714426 0.5994389    0.4342563       0.1886421
write.csv(data, "bitcoin_data.csv")
#-----------------------------------------------------------------------------
