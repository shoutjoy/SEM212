% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plspm_path.R
\name{plspm_path}
\alias{plspm_path}
\title{plspm path create}
\usage{
plspm_path(
  row_names = c("r1", "r2", "r3"),
  col_names = NULL,
  relationship = NULL
)
}
\arguments{
\item{row_names}{rowname}

\item{col_names}{colname}

\item{relationship}{input path}
}
\value{
matrix
}
\description{
plspm path create
}
\examples{

\dontrun{
edu_path <- plspm_path(
  row_names = c("Support", "Advising", "Tutoring", "Value", "Satisfaction", "Loyalty"),
  col_names = c("Support", "Advising", "Tutoring", "Value", "Satisfaction", "Loyalty"),
  relationship = list(
    list(from = "Value",
         to = c("Support", "Advising", "Tutoring")),
    list(from = "Satisfaction",
         to = c("Support", "Advising", "Tutoring","Value")),
    list(from = "Loyalty",
         to =c("Satisfaction"))
  )
)
edu_path


edu_path <- plspm_path(
  row_names = c("Support", "Advising", "Tutoring", "Value", "Satisfaction", "Loyalty"),
  col_names = c("Support", "Advising", "Tutoring", "Value", "Satisfaction", "Loyalty"),
  relationship = list(

    path(form = "Support", to ="Advising"),
    path(form = "Tutoring", to ="Advising"),
    path(from = "Value",
         to = c("Support", "Advising", "Tutoring")),
    path(from = "Satisfaction",
         to = c("Support", "Advising", "Tutoring","Value")),
    path (from = "Loyalty",
         to =c("Satisfaction"))
  )
)
edu_path

}
}
