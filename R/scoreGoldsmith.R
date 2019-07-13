#' A function that scores Goldsmith and Byer's Sexual Idealization Scale
#'
#' Calculates scale score
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "SIS_1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreGoldsmith(dat, firstq = "SIS_1")

scoreGoldsmith <- function(dat, firstq){

  itemnum<-9

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])

  varnames_scoring<-varnames
  varnames_scoring[[4]]<-paste("-", varnames_scoring[[4]], sep = "")
  varnames_scoring[[6]]<-paste("-", varnames_scoring[[6]], sep = "")
  varnames_scoring[[8]]<-paste("-", varnames_scoring[[8]], sep = "")
  varnames_scoring[[9]]<-paste("-", varnames_scoring[[9]], sep = "")

  sis.scores<-psych::scoreItems(varnames_scoring, dplyr::select(dat, !!varnames), totals = TRUE, min = 1, max = 5)
  data<-cbind(dat, sis.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"SIS"

  return(data)
}
