% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scoreDillonCounseling.R
\name{scoreDillonCounseling}
\alias{scoreDillonCounseling}
\title{A function that scores Dillon & Worthington's Lesbian, Gay, and Bisexual Affirmative Counseling
Self-Efficacy Inventory}
\usage{
scoreDillonCounseling(dat, firstq)
}
\arguments{
\item{dat}{data frame}

\item{firstq}{input character for the name of the column for the first
variable (e.g., "LGBACSEI_1")}
}
\value{
data frame with new vectors of  scores
}
\description{
Calculates five subscale scores and total score
}
\examples{
dat = scoreDillonCounseling(dat, firstq = "LGBACSEI_1")
}
