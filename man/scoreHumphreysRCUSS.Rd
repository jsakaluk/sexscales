% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scoreHumphreysRCUSS.R
\name{scoreHumphreysRCUSS}
\alias{scoreHumphreysRCUSS}
\title{A function that scores Humphreys & Kennett's Reasons for Consenting to Unwanted Sex Scale}
\usage{
scoreHumphreysRCUSS(dat, firstq)
}
\arguments{
\item{dat}{data frame}

\item{firstq}{input character for the name of the column for the first
variable (e.g., "RCUSS_1")}
}
\value{
data frame with new vectors of  scores
}
\description{
Calculates total score
}
\examples{
dat = scoreHumphreysRCUSS(dat, firstq = "RCUSS_1")
}
