% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cv_summarise_flags.R
\name{cv_summarise_flags}
\alias{cv_summarise_flags}
\title{Validation results for each sensor and variable}
\usage{
cv_summarise_flags(dat, wide = TRUE, dt = FALSE)
}
\arguments{
\item{dat}{Data frame of compiled and flagged validation data, as returned
from \code{cv_assign_tolerance_flag()}.}

\item{wide}{Logical argument indicating whether to return a wide table of
percentage values, or a long table including counts and percents.}

\item{dt}{Logical argument indicating whether to open as an interactive
datatable.}
}
\value{
Returns a summary of observations that passed and failed for each
  variable and sensor.
}
\description{
Validation results for each sensor and variable
}
