% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ICC_pois.R
\name{ICC_pois}
\alias{ICC_pois}
\title{ICC_pois  poisson model}
\usage{
ICC_pois(gmlm.poisson)
}
\arguments{
\item{gmlm.poisson}{model result}
}
\value{
possion ICC
}
\description{
ICC_pois  poisson model
}
\examples{
\dontrun{
lme4::glmer(cyl ~ wt + (wt |vs ), mtcars,
            family= poisson(link='log'))|>
            ICC_pois()
}
}
