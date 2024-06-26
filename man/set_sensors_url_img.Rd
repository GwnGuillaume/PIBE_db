% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/global.R
\name{set_sensors_url_img}
\alias{set_sensors_url_img}
\title{Set Sensors URL and Image}
\usage{
set_sensors_url_img(sensors_df)
}
\arguments{
\item{sensors_df}{The sensor DataFrame.}
}
\value{
The sensor DataFrame with added URL and image columns.
}
\description{
This function sets the URL and image columns in the sensor DataFrame 
based on the sensor location.
}
\details{
This function uses the `case_when` function from the `dplyr` package to set 
the URL and image columns (`url` and `img`) based on the sensor measurement field (`field`).
}
\examples{
\dontrun{
# Example usage
sensors_data <- read.csv("sensors_data.csv")
sensors_data <- set_sensors_url_img(sensors_data)
}
}
