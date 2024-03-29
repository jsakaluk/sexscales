% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scoreSnellAwareness.R
\name{scoreSnellAwareness}
\alias{scoreSnellAwareness}
\title{A function that scores Snell et al.'s Sexual Awareness Questionnaire}
\usage{
scoreSnellAwareness(dat, firstq)
}
\arguments{
\item{dat}{data frame}

\item{firstq}{input character for the name of the column for the first
variable (e.g., "SAS_1")}
}
\value{
data frame with new vectors of  scores
}
\description{
Calculates positive and negative total scores, as well as subscales for dominance and submission
}
\examples{
dat = scoreSnellAwareness(dat, firstq = "SAS_1")
}
