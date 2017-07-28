################################################
### MSDS Doing Data Science - Homework 11
### George Sturrock
### Volatility Analysis of the SP500 and NASDAQ
################################################

# install "tseries" if not already installed
verifyinstall <- "tseries"
new.packages <- verifyinstall[!("tseries" %in% installed.packages()[,"Package"])]
if(length(new.packages)) 
  install.packages(new.packages)

library(tseries)

## volatility function
get
Vol <- function(d, logrets)
{
	var = 0
	lam = 0
	varlist <- c()

	for (r in logrets) {
		lam = lam*(1 - 1/d) + 1

	var = (1 - 1/lam)*var + (1/lam)*r^2
		varlist <- c(varlist, var)
	}
	sqrt(varlist)
}

# Download S&P500 data
SNPdata <- get.hist.quote('^gspc',quote="Close")

# Calculate S&P500 log returns
SNPret <- log(lag(SNPdata)) - log(SNPdata)

# Calculate S&P500 volatility measure
SNPvol <- sd(SNPret) * sqrt(250) * 100

# calculate S&P500 volatility over time with three different decay factors
volest <- Vol(10,SNPret)
volest2 <- Vol(30,SNPret)
volest3 <- Vol(100,SNPret)

# Plot S&P500 volatility resutls by decay factory
plot(volest,type="l", main = "SP500 Volatility", ylab = "Volatility")
lines(volest2,type="l",col="red")
lines(volest3, type = "l", col="blue")

# Download NADSAQ data 
NSDQdata <- get.hist.quote('^ixic',quote="Close")

# Calculate NASDAQ log returns
NSDQret <- log(lag(NSDQdata)) - log(NSDQdata)

# Calculate NASDAQ volatility measure
NSDQvol <- sd(NSDQret) * sqrt(250) * 100

# calculate NASDAQ volatility over time with three different decay factors
Nvoltest <- Vol(10,NSDQret)
Nvoltest2 <- Vol(30,NSDQret)
Nvoltest3 <- Vol(100,NSDQret)

# Plot NASDAQ volatility resutls by decay factory
plot(Nvoltest,type="l", main = "Nasdaq Volatility", ylab = "Volatility")
lines(Nvoltest2,type="l",col="green")
lines(Nvoltest3, type = "l", col="orange")
