% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/global.R
\name{order_frequencies}
\alias{order_frequencies}
\title{Order Frequencies Numerically}
\usage{
order_frequencies(x)
}
\arguments{
\item{x}{A vector of strings containing frequency patterns.}
}
\value{
A factor with frequencies ordered numerically.
}
\description{
This function takes a vector of strings containing frequency
patterns, such as '100_Hz', and orders them numerically.
}
\details{
Order Frequencies Numerically
This function extracts frequencies from a vector of strings containing
patterns, such as '100_Hz', and orders them numerically.
}
\examples{
x <- c("10000_Hz", "1000_Hz", "6.3_Hz", "Z", "A")
ordered_x <- order_frequencies(x)
print(ordered_x)
}
