library(stockpick)
library(data.table)
library(magrittr)
library(IBrokers)

tws <- twsConnect()


compare_ema20("SPY", 500)
compare_ema20("QQQ", 500)

plot_tws("SPY")
