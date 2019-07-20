#' A function that scores Lottes & Quinn-Nilas's Rape Supportive Attitude Scale
#'
#' Calculates total score
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "RSAS_1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreLottesRSAS(dat, firstq = "RSAS_1")

scoreLottesRSAS <- function(dat, firstq){

  itemnum<-20

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])

  rsas.scores<-psych::scoreItems(varnames, dplyr::select(dat, !!varnames), totals = TRUE, min = 1, max = 5)
  data<-cbind(dat,rsas.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"rsas"

  return(data)
}
