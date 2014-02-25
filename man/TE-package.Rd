\name{TE-package}
\alias{TE-package}
\alias{TE}
\docType{package}
\title{
TRADING ECONOMICS for R
}
\description{
The Trading Economics R package provides its users with accurate data for 232 countries including historical data for more than 300.000 economic indicators, exchange rates, stock market indexes, government bond yields and commodity prices. You need a valid Trading Economics account to access most of the datasets. However, you can access a limited amount of data on a few indicators for free and without any registration. 
}
\details{
\tabular{ll}{
Package: \tab tradingeconomics\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-11-30\cr
License: \tab Proprietary\cr
}
The Trading Economics R package allows direct access to our databases from a variety of third-party software and custom applications. Trading Economics provides its users with accurate information for 232 countries including historical data for more than 300.000 economic indicators, exchange rates, stock market indexes, government bond yields and commodity prices. Our data is based on official sources, not third party data providers, and our facts are regularly checked for inconsistencies. TradingEconomics.com has received more than 100 million page views from more than 200 countries. To use the R api please use your own username and password instead of the guest account. The API feature is only available for paying users with a professional plan. Please upgrade your account if you haven't done so.

}
\author{
 TRADING ECONOMICS <contact@tradingeconomics.com>
}
\references{
http://api.tradingeconomics.com
}
~~ Optionally other standard keywords, one per line, from file ~~
~~ KEYWORDS in the R documentation directory ~~
\keyword{ package }
\examples{

# CONNECT
# Please replace the guest user by your email and password
# c = te.connect('guest, 'guest')

# GET COUNTRIES MATRIX
# te.get(c,'q=matrix')

# GET HISTORICAL DATA
# te.get(c,'q=historical&c=united+states&i=gdp+growth+rate')

# GET DATA FOR ONE INDICATOR | MANY COUNTRIES
# te.get(c,'q=historical&i=gdp+growth+rate&d1=2010-01-01')

# GET DATA FOR ONE COUNTRY | MANY INDICATORS
# te.get(c,'q=historical&i=gdp+growth+rate&d1=2010-01-01')

# GET G20 DATA
# g20 = te.getG20(c);

# PLOT DATA
# plot(h$Value, type='h')
# plot(as.Date(h$Date,format='%m/%d/%Y'), h$Value, type='h', xlab='Date',ylab='Value', main='Portugal GDP Growth Rate')
# plot(g20$Unemployment.Rate, g20$Inflation.Rate)
# text(g20$Unemployment.Rate, g20$Inflation.Rate, g20$Country)

# STATS
# summary(h$Value)
# hist(h$Value)

# CORRELATIONS
# pairs(g20[7:8])
# pairs(g20[3:14])
}
