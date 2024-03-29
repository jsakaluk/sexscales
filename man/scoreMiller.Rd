% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scoreMiller.R
\name{scoreMiller}
\alias{scoreMiller}
\title{A function that scores Miller & Byers's Sexual Intervention Self-Efficacy Scale
Self-Efficacy Inventory}
\usage{
scoreMiller(dat, firstq)
}
\arguments{
\item{dat}{data frame}

\item{firstq}{input character for the name of the column for the first
variable (e.g., "SIEF_1")}
}
\value{
data frame with new vectors of  scores
}
\description{
Calculates three subscale scores
}
\examples{
dat = scoreMiller(dat, firstq = "SIEF_1")
}
