#' A function that scores Catania's Dyadic Sex Communication Scale
#'
#' Calculates total score
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "DSCS_1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreCataniaDSC(dat, firstq = "DSCS_1")

scoreCataniaDSC <- function(dat, firstq){

  itemnum<-13

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])

  dsc.scores<-psych::scoreItems(varnames, dplyr::select(dat, !!varnames), totals = TRUE, min = 1, max = 6)
  data<-cbind(dat,dsc.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"dsc"

  return(data)
}
