% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scoreAalsma.R
\name{scoreAalsma}
\alias{scoreAalsma}
\title{A function that scores Aalsma & Fortenberry's Childhood Sexual Abuse
Scale (CSAS)}
\usage{
scoreAalsma(dat, firstq)
}
\arguments{
\item{dat}{data frame}

\item{firstq}{input character for the name of the column for the first
CSAS variable (e.g., "CSAS_1")}
}
\value{
data frame with new "CSAS" vector of total scores
}
\description{
Calculates total CSAS scores based on a data frame. Users specify first column
with relevant data, and function seeks out the rest automatically.
}
\examples{
dat = scoreAalsma(dat, firstq = "CSAS_1")
}
