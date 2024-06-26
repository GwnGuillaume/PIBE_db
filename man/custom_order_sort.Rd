% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/global.R
\name{custom_order_sort}
\alias{custom_order_sort}
\title{Custom sorting function for strings}
\usage{
custom_order_sort(x)
}
\arguments{
\item{x}{A character vector of strings to be sorted.}
}
\value{
A character vector of strings sorted based on the presence of digits.
}
\description{
This function returns a character vector of strings sorted based on
the presence of digits.
}
\details{
This function is designed for sorting strings based on the presence of digits.
Strings containing digits are considered greater and placed at the end, while
strings without digits are considered smaller and placed at the beginning.
}
\examples{
strings <- c("L10", "L50", "L90", "Leq")
sorted_strings <- custom_order_sort(strings)
print(sorted_strings)
}
