% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_engagement.r
\name{translate_data}
\alias{translate_data}
\title{Translate data to prep it for the engagement score calculation.}
\usage{
translate_data(organized_data_in)
}
\arguments{
\item{organized_data_in}{A list of data frames (the result of orgainizing
the data pulled from production.)}
}
\value{
A data frame (or data table) consisting of 4 columns : 
post_id reach comments
shares
}
\description{
Translate data to prep it for the engagement score calculation.
}

