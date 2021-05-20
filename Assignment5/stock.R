#Kritagya Nepal

library(quantmod)
library(tidyquant)
library(tidyverse)



getSymbols("SPY")

#--------Strategy-1 --------#

spy_stocks <- c("SPY") %>%
  tq_get(get = "stock.prices",
         from = "1990-01-01",
         to = "2021-05-07") %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "monthly",col_rename = "SPY_return")

savings = 0
spy_stocks$Strategy1_return <- NA
n <- nrow(spy_stocks)

for (i in 1:n)
{
  return_amt = spy_stocks$SPY_return[i]*savings
  investment = savings +1000
  savings = return_amt+investment
  if(i %% 12 == 0){
    spy_stocks$Strategy1_return[i] <- (savings - (1000*i))
  }
}

spy_stocks_return1 <- na.omit(spy_stocks)
print(spy_stocks_return1)
gain <- (savings - n*1000)
print(paste("Gains obtained from the investment in SPY irrespective of market up/down",gain))

#---Strategy-2----#
spy_data <- c("SPY") %>%
  tq_get(get = "stock.prices",
         from = "1990-01-01",
         to = "2021-05-07")
head(spy_data)

spy_stocks <- c("SPY") %>%
  tq_get(get = "stock.prices",
         from = "1990-01-01",
         to = "2021-05-07") %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "monthly",col_rename = "SPY_return")
head(spy_stocks)


save = 0
balance = 0
adjusted =0
ret =0

spy_data_return <- as_tibble(merge(spy_data, spy_stocks, by.spy_data = 'date', by.spy_stocks = 'date'))
spy_data_return$Strategy2_return = 0
head(spy_data_return)

n <- nrow(spy_data_return)

for(i in 2:n){
  ret <- ret + spy_data_return$SPY_return[i]
  adjusted <-  spy_data_return$adjusted[i] + ret
  if(adjusted > ret){
    if (balance == 0){
      save = save + (1000 / adjusted)}
    else{
      save = save + (balance / adjusted)
      balance = 0
    }}
  else if(adjusted < return) {
    balance = balance + 1000
    if (save > 0){
      selling = 1000 / adjusted
      save = save - selling
      if (save < 0) {
        save = 0 }
      balance = balance + (adjusted * selling)
    }}
  if (i %% 12 == 0){
    
    spy_data_return[i,"Strategy2_return"] <- (balance + (save * adjusted)  - 1000 * i)
  }
}


spy_stocks_return2 <- na.omit(spy_data_return)

gain = (balance + (save * adjusted) - 1000 * n)

print(paste("Savings made by buying the total amount of ", save, "stocks "))
print(paste("Profit made by buying stocks only when the market is up in SPY", gain))

#---Strategy 1 VS Strategy 2-----#
analysis <- cbind(spy_stocks_return1[,'Strategy1_return'], spy_stocks_return2[,c('Strategy2_return','date')])

ggplot(analysis)+ xlab('Date')+ylab ('Strategies_return')+
  geom_line(mapping = aes(x = date,y = Strategy1_return ), col = "red") +
  geom_point(mapping = aes(x = date, y = Strategy2_return), col = "green")

getSymbols("SPY")
chartSeries(SPY, from = "1990-01-01", to = "2020-04-30")
addMACD(type = "EMA")
addBBands()