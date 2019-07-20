#' A function that scores Humphreys & Kennett's Reasons for Consenting to Unwanted Sex Scale
#'
#' Calculates total score
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "RCUSS_1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreHumphreysRCUSS(dat, firstq = "RCUSS_1")

scoreHumphreysRCUSS <- function(dat, firstq){

  itemnum<-18

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])

  rcuss.scores<-psych::scoreItems(varnames, dplyr::select(dat, !!varnames), totals = TRUE, min = 0, max = 8)
  data<-cbind(dat,rcuss.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"rcuss"

  return(data)
}
