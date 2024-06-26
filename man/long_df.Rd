% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/long_df.R
\name{long_df}
\alias{long_df}
\title{longdata transfomation}
\usage{
long_df(
  data,
  names_to = "speaker",
  values_to = "freq",
  cols = 2:ncol(data1),
  rowname = "accent"
)
}
\arguments{
\item{data}{widedata}

\item{names_to}{name}

\item{values_to}{value}

\item{cols}{colrange}

\item{rowname}{defai;t acce}
}
\value{
longdata
}
\description{
longdata transfomation
}
\examples{
\dontrun{

mtcars[,1:4] \%>\%long_df("car", "value")
}
}
