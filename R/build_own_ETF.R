#' Calculate price of self-defined ETF of selected stocks
#'
#' @param etf_stocks vector of stock symbols that are components of the ETF
#' @param eft_name name of the ETF
#'
#' @export
#'

calculate_etf <- function(etf_stocks, etf_name = "ETF"){
    # calculate stock mean by date
    df <-tq_get(etf_stocks,
                 from = Sys.Date() - 730,
                 to = Sys.Date())


    dt <- as.data.table(df)

    # delete a stock if it has fewer than 200 rows
    symbol_more_than200 <- dt[, .(count = .N), by = symbol] %>%
        .[count > 200, symbol]
    dt <- dt[symbol %in% symbol_more_than200]
    df <- as.data.frame(dt)

    # fill in NA with values from previous trading day
    while (TRUE){
        n_NA_before <- sum(is.na(dt))
        for (col in c("open", "high", "low", "close", "adjusted", "volume")){
            dt[, (col) := ifelse(!is.na(get(col)), get(col), shift(get(col))),
               by = symbol]
        }
        if (sum(is.na(dt)) == 0) break
        # if first date has missing value
        if (sum(is.na(dt)) == n_NA_before) break
    }


    # if first date has missing value, fill value from next day
    while (TRUE){
        n_NA_before <- sum(is.na(dt))
        for (col in c("open", "high", "low", "close", "adjusted", "volume")){
            dt[
                , (col) := ifelse(
                    !is.na(get(col)), get(col), shift(get(col), type = "lead"
                    )),
                by = symbol
                ]
        }
        if (sum(is.na(dt)) == 0) break
        # if missing value in other columns, just stop
        if (sum(is.na(dt)) == n_NA_before) break
    }

    # calculate etf price
    dt <- dt %>%
        # weighted by mean volume
        .[, .SD / max(close), by = symbol,
          .SDcols = c("open", "high", "low", "close", "adjusted")] %>%
        .[, date := df$date] %>%
        .[, volume := df$volume] %>%
        .[, symbol := NULL] %>%
        .[, lapply(.SD, function(x) round(mean(x), 3)), by = date] %>%
        # normalize by first trading date
        .[, ":=" (
            open = round(open / close[1], 3),
            high = round(high / close[1], 3),
            low = round(low / close[1], 3),
            close = round(close / close[1], 3),
            adjusted = round(adjusted / close[1], 3)
        )] %>%
        .[, symbol := etf_name] %>%
        setcolorder(names(df))

    return(dt)
}


#' Plot the chart of self-defined ETF
#'
#' @param etf_stocks vector of stock symbols that are components of the ETF
#' @param eft_name name of the ETF
#'
#' @export
#'

plot_etf <- function(etf_stocks, etf_name = "ETF"){
    etf <- calculate_etf(etf_stocks, etf_name)
    assign(etf_name, tq2qm(etf))
    chartSeries(get(etf_name), name = etf_name,
                TA = c(addMACD(12, 26, 9, histogram = FALSE),
                       addVo(),
                       addEMA(15, col = "orange"),
                       addEMA(6, col = "green")))
}
