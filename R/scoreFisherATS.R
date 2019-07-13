#' A function that scores Fisher's Attitude Towards Sexuality Scale
#'
#' Calculates scale score
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "ATSS_1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreFisherATS(dat, firstq = "ATSS_1")

scoreFisherATS <- function(dat, firstq){

  itemnum<-13

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])

  varnames_scoring<-varnames
  varnames_scoring[[1]]<-paste("-", varnames_scoring[[1]], sep = "")
  varnames_scoring[[4]]<-paste("-", varnames_scoring[[4]], sep = "")
  varnames_scoring[[5]]<-paste("-", varnames_scoring[[5]], sep = "")
  varnames_scoring[[7]]<-paste("-", varnames_scoring[[7]], sep = "")
  varnames_scoring[[8]]<-paste("-", varnames_scoring[[8]], sep = "")
  varnames_scoring[[11]]<-paste("-", varnames_scoring[[11]], sep = "")
  varnames_scoring[[13]]<-paste("-", varnames_scoring[[13]], sep = "")

  ats.scores<-psych::scoreItems(varnames_scoring, dplyr::select(dat, !!varnames), totals = TRUE, min = 1, max = 5)
  data<-cbind(dat, ats.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"ATS"

  return(data)
}
