#' A function that scores McDonagh & Morrison's Male Body Image Self-Consciousness Scale
#'
#' Calculates total score
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "MBISCS_1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreMcDonaghMBISC(dat, firstq = "MBISCS_1")

scoreMcDonaghMBISC <- function(dat, firstq){

  itemnum<-17

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])

  total.scores<-psych::scoreItems(varnames, dplyr::select(dat, !!varnames), totals = TRUE, min = 1, max = 5)
  data<-cbind(dat, total.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"MBISC.total"

  return(data)
}
