% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datapate.R
\name{datapaste}
\alias{datapaste}
\title{exceldata to dataframe}
\usage{
datapaste(type = "matrix", x = "clipboard")
}
\arguments{
\item{type}{"clipboard" to table data. dataframe, tibble}

\item{x}{"clipboard"}
}
\value{
paste data.frame
}
\description{
exceldata to dataframe
}
\examples{
#first you must copy data (Ctrl+C)
## then excute below ctrl+Enter
df = datapaste()
df = datapaste("tibble")
df = datapaste("table")
df = datapaste("data.frame")

}
