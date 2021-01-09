#' plot chart of stocks
#'
#' @param stocks vector of stock symbols such as c("C", "AAPL")
#' @param period number of trading day to display
#' @param description description of the chart, stock symbol by default
#'
#' @export

display_stocks <- function(stocks, period = 350, description = NULL){
    for (i in seq_along(stocks)){
        st <- stocks[i]
        cat(st)
        stock <- getSymbols(st, auto.assign = FALSE) %>%
            na.omit()    # get rid of missing values

        if (nrow(stock) > period){
            stock <- stock[(nrow(stock) - period + 1) : nrow(stock)]
        }

        if (!is.null(description)){
            name <- paste0(st, ", ", description[i])
        } else {
            name = st
        }

        # set the first day's price high at 2 times of minimum so that it is
        # easy to see a narrow range
        min_low <- min(stock[, 3])
        max_high <- max(stock[, 2])
        if (max_high < 2 * min_low) {
            stock[1, 2] <- 2 * min_low
        }

        if (nrow(stock) == period){
            chartSeries(stock,
                        TA = c(addVo(),
                               addMACD(12, 26, 9, histogram = FALSE),
                               addEMA(200, col = "red"),
                               addEMA(50, col = "yellow"),
                               #addEMA(20, col = "green"),
                               # add functions does not take variables
                               addLines(v = c(50, 175),
                                        col = "grey27")),
                        name = name)
        } else if (nrow(stock) > 200){
            chartSeries(stock,
                        TA = c(addVo(),
                               addMACD(12, 26, 9, histogram = FALSE),
                               addEMA(200, col = "red"),
                               addEMA(50, col = "yellow"),
                               #addEMA(20, col = "green")
                               ),
                        name = name)
        } else if (nrow(stock) > 50){
            chartSeries(stock,
                        TA = c(addVo(),
                               addMACD(12, 26, 9, histogram = FALSE),
                               addEMA(50, col = "yellow"),
                               #addEMA(20, col = "green")
                               ),
                        name = name)
        } else {
            chartSeries(stock, name = name)
        }


        if (i == length(stocks)) break

        key <- readline(
            prompt = paste0("Press [Enter] to view ", i + 1, "/", length(stocks),
                            " stock, [Esc] to exit: ")
        )
    }
}


#' Plot revenue growth of stocks
#'
#' @param stocks vector of stock symbols such as c("C", "AAPL")
#'
#' @export
#'
plot_revenue_growth <- function(stocks){
    dt <- get_revenues(stocks)
    p <- ggplot(dt, aes(date, change_pct, color = symbol)) +
        geom_line() +
        geom_point()
    print(p)
}
