#' A function that scores Buunk's Types of Jealousy Scale
#'
#' Calculates Reactive, Preventative, and Anxious subscale scores
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "APCBSSI_1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreBuunk(dat, firstq = "APCBSSI_1")

scoreBuunk <- function(dat, firstq){

  itemnum<-15

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])

  reactive.names<-varnames[1:5]
  preventative.names<-varnames[6:10]
  anxious.names<-varnames[11:15]

  reactive.scores<-psych::scoreItems(reactive.names, dplyr::select(dat, !!reactive.names), totals = TRUE, min = 1, max = 5)
  data<-cbind(dat, reactive.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"reative"

  preventative.scores<-psych::scoreItems(preventative.names, dplyr::select(dat, !!preventative.names), totals = TRUE, min = 1, max = 5)
  data<-cbind(data, preventative.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"preventative"

  anxious.scores<-psych::scoreItems(anxious.names, dplyr::select(dat, !!anxious.names), totals = TRUE, min = 1, max = 5)
  data<-cbind(data, anxious.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"anxious"

  return(data)
}
