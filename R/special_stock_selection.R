#' Download historical stock prices
#'
#'
#' @param stocks a vector of stocks to select from
#' @param past_days integer, download data from a few days ago
#' @return a data.table
#'
#' @export
#'
download_stock_data <- function(stocks, past_days = 7){
    # get one week data first then select last n_days
    df <- tq_get(stocks,
                 get = "stock.prices",
                 from = Sys.Date() - past_days,
                 to = Sys.Date()) %>%
        setDT()
    return(df)
}


#' Find jumping stocks
#'
#' @description To search for stocks whose price rose or fell beyond a certain
#' value in the past n trading days.
#'
#' @param stock_data stock data downloaded with function download_stock_data()
#' @param n_days past n trading days
#' @param win_cut,lose_cut cutoff percent to be considered as jumping stocks
#' @return a list of winners and losers
#'
#' @export
#'
get_jumper <- function(stock_data, n_days =1, win_cut = 10, lose_cut = -10){
    # get one week data first then select last n_days
    df <- stock_data %>%
        .[, change := c(0, diff(close)) / shift(close) * 100, by = symbol] %>%
        .[, .SD[c((.N - n_days + 1) : .N)], by = symbol]

    winner <- df[change > win_cut, unique(symbol)]

    loser <- df[change < lose_cut, unique(symbol)]

    return(list(winner = winner, loser = loser))
}


#' Get actively traded stocks
#'
#' @description Get actively traded stocks whose average daily trading volume is above 100k
#' in the past week
#'
#' @param stock_data stock data downloaded with function download_stock_data()
#'
#' @return a vector of stocks
#'
#' @export
#'
get_active_stock <- function(stock_data){
    stocks <- stock_data[, avg_vol := SMA(volume, 10), by = .(symbol)] %>%
        .[, .SD[.N], by = .(symbol)] %>%
        .[volume > 3 * avg_vol, symbol]

    return(stocks)
}



#' Find reversal stocks
#'
#' @description To search for stocks that was sold off and then becomes stable.
#' Watch them for reversal.
#' @param stocks a vector of stocks to select from
#' @param n1,n2 n1 and n2 days back from today
#' @param fraction the ratio of the trading range between n1 and n2 days to
#' that of the past two years
#'
#' @return A vector of seleced stocks
#'
#' @export
#'
get_reversals <- function(stocks, n1 = 30, n2 = 180, fraction = 0.3){

    if (!exists("stock_df")){
        # keep downloaded data in working environment for the session
        stock_df <- tq_get(stocks,
                           get = "stock.prices",
                           from = Sys.Date() - 730,
                           to = Sys.Date())
    }

    # highs and lows of stocks in the range of n2 ~ n1 days back from today
    df2 <- stock_df[date > Sys.Date() - n2 & date < Sys.Date() - n1] %>%
        .[, ":=" (max2 = max(high, na.rm = TRUE),
                  min2 = min(low, na.rm = TRUE),
                  mean2 = mean(close, na.rm = TRUE)),
          by = symbol] %>%
        .[, .(symbol, max2, min2, mean2)] %>%
        unique()

    # df2 <- filter(stock_df, date > Sys.Date() - n2 & date < Sys.Date() - n1) %>%
    #     group_by(symbol) %>%
    #     mutate(max2 = max(high, na.rm = TRUE),
    #            min2 = min(low, na.rm = TRUE),
    #            mean2 = mean(close, na.rm = TRUE)) %>%
    #     select(symbol, max2, min2, mean2) %>%
    #     unique()

    # highs and lows in the past n2 days
    df3 <- stock_df[date < Sys.Date() - n2] %>%
        .[, ":=" (max3 = max(high, na.rm = TRUE),
                  min3 = min(low, na.rm = TRUE),
                  mean3 = mean(close, na.rm = TRUE)),
          by = symbol] %>%
        .[, .(symbol, max3, min3, mean3)] %>%
        unique()

    # df3 <- filter(stock_df, date < Sys.Date() - n2) %>%
    #     group_by(symbol) %>%
    #     mutate(max3 = max(high, na.rm = TRUE),
    #            min3 = min(low, na.rm = TRUE),
    #            mean3 = mean(close, na.rm = TRUE)) %>%
    #     select(symbol, max3, min3, mean3) %>%
    #     unique()

    reversals <- left_join(df2, df3, by = "symbol") %>%
        filter(mean2 < 0.3 * max3) %>%
        filter((max2 - min2) < fraction * (max3 - min(min3, min2))) %>%
        pull(symbol)

    return(reversals)
}

#' Find growth stocks
#' @description search for stocks that have revenue growth rate above cut_off
#' each year in last three years
#'
#' @param stocks : vector of stocks to search within
#' @param cut_off : cut off ratio of revenue growth percent
#'
#' @return a vector of stock symbol
#'
#' @export
#'
get_growth <- function(stocks, cut_off = 5){
    dt <- get_revenues(stocks) %>%
        .[, .SD[c(.N -2, .N -1, .N)], by = symbol] %>%
        .[, .(grow = all(change_pct > cut_off)), by = symbol] %>%
        .[grow == TRUE, symbol]

    return(dt)
}
