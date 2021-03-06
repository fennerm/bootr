% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{split_table}
\alias{split_table}
\title{Split a data.frame by a column}
\usage{
split_table(df, by = NULL, levels = NULL)
}
\arguments{
\item{df}{A data.frame}

\item{by}{A column name}

\item{levels}{A set of possible values in by. If NULL, the table is
split on all unique values.}
}
\value{
A list of data.frames
}
\description{
Split a data.frame by a column
}
