#' Display the percentage difference of stock close price to EMA20
#'
#' @param stock stock symbol
#' @param n_days number of days to display
#' @export
#'

compare_ema20 <- function(stock, n_days = 1000){
    exch <- stock_exchange[symbol == stock, exchange]
    if (stock %in% c("SPY", "QQQ", "DIA", "GLD", "XRT", "IYR", "XLU", "XLV", "XLE", "XLB",
                     "TLT", "IWM", "HYG", "KBE", "XLK", "XLP", "XLI", "KRE", "XLY", "SMH",
                     "VNQ", "XBI", "ITB", "KWEB", "IBB", "SOXX", "FXR", "FDN", "VGT",
                     "SCHH", "IYZ")){
        exch <- "NYSE"
    }

    symbol <- twsSTK(stock, primary = exch)

    data = reqHistoricalData(tws, symbol, duration = "10 Y") %>%
        na.omit() %>%   # get rid of missing values
        .[, 3] %>%
        as.data.table() %>%
        setnames(c("date", "close")) %>%
        .[, ema20 := EMA(close, 20)] %>%
        .[, ema50 := EMA(close, 50)] %>%
        .[, ema200 := EMA(close, 200)] %>%
        .[, diff20 := round(100 * (close - ema20) / ema20, 2)] %>%
        .[, trend := ifelse(ema50 >= ema200, "up", "down")] %>%
        .[(.N - n_days):.N]

    g <- ggplot(data, aes(date, diff20)) +
        geom_point(aes(color = trend), size = 0.5) +
        geom_line(alpha = 0.3) +
        scale_color_manual(values = c("up" = "green", "down" = "red")) +
        labs(title = stock)
    print(g)
}

