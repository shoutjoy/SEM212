% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table_df.R
\name{table_df}
\alias{table_df}
\title{Frequency Analysis Functions}
\usage{
table_df(data, remove = NULL, digits = 4)
}
\arguments{
\item{data}{data.frame}

\item{remove}{Select variables to exclude from analysis}

\item{digits}{round}
}
\value{
Publication frequency table
}
\description{
Frequency Analysis Functions
}
\examples{

\dontrun{
## Identifying variables
 mtcars \%>\% as_trt("cyl","am","vs","gear")\%>\% tibble()

 mtcars \%>\% as_trt("cyl","am","vs","gear")\%>\% table_df()

 iris \%>\% as_trt("Species")\%>\% table_df()
}


}
