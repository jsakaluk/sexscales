#' A function that scores Janssen et al.'s Sexual Excitation/Sexual Inhibition Inventory (short-form)
#'
#' Subscale total scores
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "sesimw1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreMilhausenSESIMW(dat, firstq = "sesimw1")

scoreMilhausenSESIMW <- function(dat, firstq){

  itemnum<-30

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])

  inhibcond.names <-varnames[1:8]
  inhibcond.scores<-psych::scoreItems(inhibcond.names, dplyr::select(dat, !!inhibcond.names), totals = FALSE, min = 1, max = 4)
  data<-cbind(dat, inhibcond.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"inhibcond"

  relimport.names <-varnames[9:13]
  relimport.names_scoring <-relimport.names
  relimport.names_scoring[[3]]<-paste("-", relimport.names_scoring[[3]], sep = "")
  relimport.scores<-psych::scoreItems(relimport.names_scoring, dplyr::select(dat, !!relimport.names), totals = FALSE, min = 1, max = 4)
  data<-cbind(data, relimport.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"relimport"

  arouse.names <-varnames[14:18]
  arouse.scores<-psych::scoreItems(arouse.names, dplyr::select(dat, !!arouse.names), totals = FALSE, min = 1, max = 4)
  data<-cbind(data, arouse.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"arouse"

  partnerchar.names <-varnames[19:23]
  partnerchar.scores<-psych::scoreItems(partnerchar.names, dplyr::select(dat, !!partnerchar.names), totals = FALSE, min = 1, max = 4)
  data<-cbind(data, partnerchar.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"partnerchar"

  setting.names <-varnames[24:27]
  setting.names_scoring <-setting.names
  setting.names_scoring[[1]]<-paste("-", setting.names_scoring[[1]], sep = "")
  setting.names_scoring[[3]]<-paste("-", setting.names_scoring[[3]], sep = "")
  setting.scores<-psych::scoreItems(setting.names_scoring, dplyr::select(dat, !!setting.names), totals = FALSE, min = 1, max = 4)
  data<-cbind(data, setting.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"setting"

  dyadic.names <-varnames[28:30]
  dyadic.scores<-psych::scoreItems(dyadic.names, dplyr::select(dat, !!dyadic.names), totals = FALSE, min = 1, max = 4)
  data<-cbind(data, dyadic.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"dyadic"

  return(data)
}
