install.packages("quantmod")
library(quantmod)
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
getSymbols("MSFT", auto.assign = F)
MSFT.prices <- getSymbols("MSFT", auto.assign = F)
MSFT.prices <- Ad(getSymbols("MSFT", auto.assign = F))
maxDate <- "2000-01-01"
MSFT.prices <- Ad(getSymbols("MSFT", auto.assign = F, from=maxDate))
head(MSFT.prices)
tail(MSFT.prices)
MSFT.rets <- dailyReturn(MSFT.prices)
VaR(MSFT.rets, p=0.95, method = "historical")
VaR(MSFT.rets, p=0.99, method = "historical")
CVaR(MSFT.rets, p=0.95, method = "historical")
tickers <- c("MSFT", "AAPL", "AMZN")
weights <- c(0.5, 0.1, 0.4)
getSymbols(tickers, from=maxDate)
Port.prices <- merge(Ad(MSFT), Ad(AAPL), Ad(AMZN))
head(Port.prices)
Port.prices <- na.omit(merge(Ad(MSFT), Ad(AAPL), Ad(AMZN)))
Port.returns <- ROC(Port.prices, type = "discrete")
head(Port.returns)
Port.returns <- ROC(Port.prices, type = "discrete")
head(Port.returns)
Port.returns <- ROC(Port.prices, type = "discrete")[-1,]
head(Port.returns)
colnames(Port.returns) <- tickers
head(Port.returns)
VaR(Port.returns, p=0.99, weights=weights, portfolio_method = "component", method = "modified")
?VaR
ES(Port.returns, p=0.99, weights=weights, portfolio_method = "component", method = "modified")
ETL(Port.returns, p=0.99, weights=weights, portfolio_method = "component", method = "modified")
VAR.Hist <- VaR(Port.returns, p=0.95, weights = NULL, portfolio_method = "single", method = "historical")
VAR.Hist
VAR.Gaus <- VaR(Port.returns, p=0.95, weights = NULL, portfolio_method = "single", method = "historical")
VAR.Gaus
VAR.Mod <- VaR(Port.returns, p=0.95, weights = NULL, portfolio_method = "single", method = "historical")
VAR.Mod
All.VAR <- data.frame(rbind(VAR.Hist, VAR.Gaus, VAR.Mod))
rownames(All.VAR) <- c("Hist", "Gaus", "Mod")
All.VAR
PortVAR.Hist <- VaR(Port.returns, p=0.95, weights = weights, portfolio_method = "component", method = "historical")
PortVAR.Hist
PortVAR.Gaus <- VaR(Port.returns, p=0.95, weights = weights, portfolio_method = "component", method = "guassian")
PortVAR.Gaus
PortVAR.Mod <- VaR(Port.returns, p=0.95, weights = weights, portfolio_method = "component", method = "modified")
PortVAR.Mod
Port.Gaus <- VaR(Port.returns, p=0.95, weights = weights, portfolio_method = "component", method = "guassian")$VaR[1,1]
Port.Gaus
Port.Mod <- VaR(Port.returns, p=0.95, weights = weights, portfolio_method = "component", method = "modified")$MVaR[1,1]
Port.Mod
All.VAR
All.VAR$Portfolio <- 0
All.VAR
All.VAR$Type <- c("Hist", "Gaus", "Mod")
install.packages("ggplot2")
library(ggplot2)
install.packages("reshape2")
library("ggplot2")
plotVAR <- melt(All.VAR)
plotVAR
plotVAR <- melt(All.VAR, variable.names = "Ticker", value.name = "VaR")
plotVAR
ggplot(plotVAR, aes(x=Type, y=VaR, fill=Ticker)) + geom_bar(stat = "identity", position = "dodge")
