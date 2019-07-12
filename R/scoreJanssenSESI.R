#' A function that scores Janssen et al.'s Sexual Excitation/Sexual Inhibition Inventory
#'
#' Subscale average scores and higher order average scores
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "SESIW1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreJanssenSESI(dat, firstq = "SESIW1")

scoreJanssenSESI <- function(dat, firstq){

  itemnum<-45

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])

  ses.names <-varnames[c(1,3,4,6,7,11,13,14,16,25,26,29,30,32,35,37,38,39,43,44)]
  ses.names_scoring <-ses.names
  for(i in 1:length(ses.names_scoring)){
    ses.names_scoring[[i]]<-paste("-", ses.names_scoring[[i]], sep = "")
  }
  ses.scores<-psych::scoreItems(ses.names_scoring, dplyr::select(dat, !!ses.names), totals = FALSE, min = 1, max = 4)
  data<-cbind(dat, ses.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"ses"

  sis1.names <-varnames[c(5,9,10,17,19,20,21,23,33,36,40,41,42,45)]
  sis1.names_scoring <-sis1.names
  for(i in 1:3){
    sis1.names_scoring[[i]]<-paste("-", sis1.names_scoring[[i]], sep = "")
  }
  for(i in 5:13){
    sis1.names_scoring[[i]]<-paste("-", sis1.names_scoring[[i]], sep = "")
  }
  sis1.scores<-psych::scoreItems(sis1.names_scoring, dplyr::select(dat, !!sis1.names), totals = FALSE, min = 1, max = 4)
  data<-cbind(data, sis1.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"sis1"

  sis2.names <-varnames[c(2,8,12,15,18,22,24,27,28,31,34)]
  sis2.names_scoring <-sis2.names
  for(i in 1:length(sis2.names_scoring)){
    sis2.names_scoring[[i]]<-paste("-", sis2.names_scoring[[i]], sep = "")
  }
  sis2.scores<-psych::scoreItems(sis2.names_scoring, dplyr::select(dat, !!sis2.names), totals = FALSE, min = 1, max = 4)
  data<-cbind(data, sis2.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"sis2"

  return(data)
}
