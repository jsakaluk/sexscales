% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scoreLottesRSAS.R
\name{scoreLottesRSAS}
\alias{scoreLottesRSAS}
\title{A function that scores Lottes & Quinn-Nilas's Rape Supportive Attitude Scale}
\usage{
scoreLottesRSAS(dat, firstq)
}
\arguments{
\item{dat}{data frame}

\item{firstq}{input character for the name of the column for the first
variable (e.g., "RSAS_1")}
}
\value{
data frame with new vectors of  scores
}
\description{
Calculates total score
}
\examples{
dat = scoreLottesRSAS(dat, firstq = "RSAS_1")
}
