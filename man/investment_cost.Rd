% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/investment_cost.R
\name{investment_cost}
\alias{investment_cost}
\title{Calculate the trading cost during fund investment}
\usage{
investment_cost(raw)
}
\arguments{
\item{raw}{Text content from pdf file}
}
\value{
Investment cost data
}
\description{
Calculate the trading cost during fund investment
}
\examples{
data(pdf_content)
investment_cost(pdf_content)
}
