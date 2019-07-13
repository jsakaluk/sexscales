#' A function that scores Catania's Dyadic Sex Regulation Scale
#'
#' Calculates scale score
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "DSRS_1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreCataniaDySexReg(dat, firstq = "DSRS_1")

scoreCataniaDySexReg <- function(dat, firstq){

  itemnum<-11

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])
  varnames_scoring<-varnames
  varnames_scoring[[2]]<-paste("-", varnames_scoring[[2]], sep = "")
  varnames_scoring[[5]]<-paste("-", varnames_scoring[[5]], sep = "")
  varnames_scoring[[6]]<-paste("-", varnames_scoring[[6]], sep = "")
  varnames_scoring[[8]]<-paste("-", varnames_scoring[[8]], sep = "")
  varnames_scoring[[10]]<-paste("-", varnames_scoring[[10]], sep = "")

  dsr.scores<-psych::scoreItems(varnames_scoring, dplyr::select(dat, !!varnames), totals = TRUE, min = 1, max = 7)
  data<-cbind(dat, dsr.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"DySexReg"

  return(data)
}
