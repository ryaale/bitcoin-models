# Import files as xts
# Import btc price [as xts object]
price.zoo <- read.zoo(file ="~/Desktop/Crypto Market Analysis/market-price.csv", sep=",", header=FALSE, format="%Y-%m-%d %H:%M:%S", drop=FALSE)
btc.price <- as.xts(price.zoo)
colnames(btc.price) <- c("btc.price")
#---------------------------------------------------------------------------------------------
# Import tx vol [as xts object]
tx.vol.zoo <- read.zoo(file ="~/Desktop/Crypto Market Analysis/n-transactions.csv", sep=",", header=FALSE, format="%Y-%m-%d %H:%M:%S", drop=FALSE)
tx.vol <- as.xts(tx.vol.zoo)
colnames(tx.vol) <- c("tx.vol")
#---------------------------------------------------------------------------------------------
# Import exch vol [as xts object]
exch.vol <- as.xts(btc$`exchangeVolume(USD)`, order.by = btc$date)
colnames(exch.vol) <- c("exch.vol")
#---------------------------------------------------------------------------------------------
# Import tot wallets [as xts object]
tot.wallets.zoo <- read.zoo(file ="~/Desktop/Crypto Market Analysis/my-wallet-n-users.csv", sep=",", header=FALSE, format="%Y-%m-%d %H:%M:%S", drop=FALSE)
tot.wallets <- as.xts(tot.wallets.zoo)
colnames(tot.wallets) <- c("tot.wallets")
#---------------------------------------------------------------------------------------------
# Import tot.unique.addr [as xts object]
tot.unique.addr.zoo <- read.zoo(file ="~/Desktop/Crypto Market Analysis/n-unique-addresses.csv", sep=",", header=FALSE, format="%Y-%m-%d %H:%M:%S", drop=FALSE)
tot.unique.addr <- as.xts(tot.unique.addr.zoo)
colnames(tot.unique.addr) <- c("tot.unique.addr")
#---------------------------------------------------------------------------------------------
# Import hash rate [as xts object]
tot.hashrate.zoo <- read.zoo(file ="~/Desktop/Crypto Market Analysis/hash-rate.csv", sep=",", header=FALSE, format="%Y-%m-%d %H:%M:%S", drop=FALSE)
tot.hashrate <- as.xts(tot.hashrate.zoo)
colnames(tot.hashrate) <- c("tot.hashrate")
#---------------------------------------------------------------------------------------------
# Merge xts files into one xts file, called btc.xts
q <- merge(btc.price, tx.vol, join = 'left')
e <- merge(q, exch.vol, join = 'left')
r <- merge(e, tot.wallets, join = 'left')
t <- merge(r, tot.unique.addr, join = 'left')
btc.xts <- merge(t, tot.hashrate, join = 'left')
#----------------------------------------------------------------------------
##### Set date parameters for data file #####
date <- "2014-01-01/"
data1 <- na.approx(btc.xts[date]) # NAs Linearly approximated
d <- data.frame(date=index(data1), coredata(data1))
d <- d[2:10]
d <- na.omit(d)
write.csv(x = d, file = "bitcoin_data")
data <- log(data1) # apply log scale