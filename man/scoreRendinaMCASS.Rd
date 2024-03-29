% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scoreRendinaMCASS.R
\name{scoreRendinaMCASS}
\alias{scoreRendinaMCASS}
\title{A function that scores Rendina et al.'s Maladaptive Cognitions About Sex Scale}
\usage{
scoreRendinaMCASS(dat, firstq)
}
\arguments{
\item{dat}{data frame}

\item{firstq}{input character for the name of the column for the first
variable (e.g., "MCASS_1")}
}
\value{
data frame with new vectors of  scores
}
\description{
Calculates positive and negative total scores, as well as subscales for dominance and submission
}
\examples{
dat = scoreRendinaMCASS(dat, firstq = "MCASS_1")
}
