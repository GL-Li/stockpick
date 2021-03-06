% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/basic_stock_selection.R
\name{select_cap}
\alias{select_cap}
\title{Select from a pool of stocks whose market caps are in the given range}
\usage{
select_cap(stocks, from, to)
}
\arguments{
\item{stocks}{a subset of all stocks generated by \link{get_all_stocks}}

\item{from, to}{lower and upper limits of market cap to be selected.}
}
\value{
a data.table
}
\description{
Select from a pool of stocks whose market caps are in the given range
}
