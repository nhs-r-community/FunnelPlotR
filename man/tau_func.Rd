% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/od_adjustment.R
\name{tau_func}
\alias{tau_func}
\title{Calculate the between group standard error (tau2) using a dispersion factor}
\usage{
tau_func(n, phi, S)
}
\arguments{
\item{n}{The number of groups for data items, e.g. hospitals trusts that
z-scores are calculated at.}

\item{phi}{The dispersion ratio, where > 1 means overdispersion}

\item{S}{Standard error (within cluster, calculated in z-score process)}
}
\value{
A numeric Tau2 value
}
\description{
Internal function to calculate the additional, between group
, standard error (tau2) to add to S2.
}
\keyword{internal}
