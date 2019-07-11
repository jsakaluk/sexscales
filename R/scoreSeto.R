#' A function that scores Seto et al.'s Revised Screening Scale for Pedophilic Interests
#'
#' Calculates total SPPI-2 scores based on a data frame. Users specify first column
#' with relevant data, and function seeks out the rest automatically.
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#' SPPI variable (e.g., "RSS_1")
#' @return data frame with new "SPPI2" vector of total scores
#' @export
#' @examples
#' dat = scoreSeto(dat, firstq = "RSS_1")

scoreSeto <- function(dat, firstq){

  itemnum<-5

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])

  sppi.scores<-psych::scoreItems(varnames, dplyr::select(dat, !!varnames), totals = TRUE, min = 0, max = 1)
  data<-cbind(dat, sppi.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"SPPI2"

  return(data)
}
