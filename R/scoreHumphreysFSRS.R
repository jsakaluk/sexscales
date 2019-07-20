#' A function that scores Humphreys & Kennett's Female Sexual Resourcefulness Scale
#'
#' Calculates total score
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "FSRS_1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreHumphreysFSRS(dat, firstq = "FSRS_1")

scoreHumphreysFSRS <- function(dat, firstq){

  itemnum<-19

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])
  varnames_scoring <- varnames
  varnames_scoring[[2]]<-paste("-", varnames_scoring[[2]], sep = "")
  varnames_scoring[[3]]<-paste("-", varnames_scoring[[3]], sep = "")
  varnames_scoring[[5]]<-paste("-", varnames_scoring[[5]], sep = "")
  varnames_scoring[[6]]<-paste("-", varnames_scoring[[6]], sep = "")
  varnames_scoring[[7]]<-paste("-", varnames_scoring[[7]], sep = "")
  varnames_scoring[[16]]<-paste("-", varnames_scoring[[16]], sep = "")
  varnames_scoring[[17]]<-paste("-", varnames_scoring[[17]], sep = "")
  varnames_scoring[[18]]<-paste("-", varnames_scoring[[18]], sep = "")

  fsrs.scores<-psych::scoreItems(varnames_scoring, dplyr::select(dat, !!varnames), totals = TRUE, min = 1, max = 6)
  data<-cbind(dat,fsrs.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"fsrs"

  return(data)
}
