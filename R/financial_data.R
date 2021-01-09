#' Download annual financial data
#'
#' @description Including revenue, margin, income, dividends, earnings,
#' cash flow, capital, ... ...
#'
#' @param stocks vector of stock symbols such as c("C", "AAPL")
#' @return a data.table
#'
#' @examples
#' aaa <- get_financials(c("AAPL", "C"))
#'
#' @export

get_financials <- function(stocks){
    # get annual financials data
    if (length(stocks) == 1){
        dt <- tq_get(stocks, "key.ratios")
        if (!all(is.na(dt))) {
            dt <- setDT(dt) %>%
                .[, symbol := stocks] %>%
                .[section == "Financials", .(symbol, data)]
        }
    } else {
        dt <- tq_get(stocks, "key.ratios")
        if (!all(is.na(dt))){
            dt <- setDT(dt) %>%
                .[section == "Financials", .(symbol, data)]
        }
    }

    if (all(is.na(dt))){
        message("No data available")
        return(NULL)
    } else {
        # some stocks miss key.ratios, so only keep those have
        not_missing <- dt[, symbol]
        missing <- setdiff(stocks, not_missing)

        if (!is_empty(missing)) {
            message(paste0("missing stocks: ", paste(missing, collapse = ",")))
        }


        dt <- dt[, data]

        # turn data for each stock into a data.table

        for (i in seq_along(not_missing)){
            st <- not_missing[i]
            financial <- dt[[i]] %>%
                setDT() %>%
                .[, symbol := st]
            assign(st, financial)
        }

        # form a big data.table
        combined <- rbindlist(mget(not_missing))
        return(combined)
    }

}


#' Get annual revenues and caculate rate of changes
#'
#' @param stocks vector of stock symbols such as c("C", "AAPL")
#' @return a data.table
#'
#' @examples
#' aaa <- get_revenues(c("AAPL", "C"))

#'
#' @export
#'

get_revenues <- function(stocks){
    dt <- get_financials(stocks) %>%
        .[group == 1, .(symbol, date, revenue = value)] %>%
        .[, change_pct := round(c(0, diff(revenue)) / shift(revenue) * 100, 2), by = symbol]

    return(dt)
}

