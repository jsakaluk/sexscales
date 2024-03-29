% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scoreNapper.R
\name{scoreNapper}
\alias{scoreNapper}
\title{A function that scores Napper's Negative Impact of Hookups Inventory}
\usage{
scoreNapper(dat, firstq)
}
\arguments{
\item{dat}{data frame}

\item{firstq}{input character for the name of the column for the first
variable (e.g., "NIHI_1")}
}
\value{
data frame with new vectors of  scores
}
\description{
Calculates scale score scores
}
\examples{
dat = scoreNapper(dat, firstq = "NIHI_1")
}
