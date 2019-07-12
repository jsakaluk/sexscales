#' A function that scores Janssen et al.'s Sexual Excitation/Sexual Inhibition Inventory (short-form)
#'
#' Subscale total scores
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "sesi1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreJanssenSESI_SF(dat, firstq = "sesi1")

scoreJanssenSESI_SF <- function(dat, firstq){

  itemnum<-14

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])

  ses.names <-varnames[c(1,3,8,10,11,14)]
  ses.names_scoring <-ses.names
  for(i in 1:length(ses.names_scoring)){
    ses.names_scoring[[i]]<-paste("-", ses.names_scoring[[i]], sep = "")
  }
  ses.scores<-psych::scoreItems(ses.names_scoring, dplyr::select(dat, !!ses.names), totals = TRUE, min = 1, max = 4)
  data<-cbind(dat, ses.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"ses"

  sis1.names <-varnames[c(4,9,12,13)]
  sis1.names_scoring <-sis1.names
  for(i in 1:length(sis1.names_scoring)){
    sis1.names_scoring[[i]]<-paste("-", sis1.names_scoring[[i]], sep = "")
  }
  sis1.scores<-psych::scoreItems(sis1.names_scoring, dplyr::select(dat, !!sis1.names), totals = TRUE, min = 1, max = 4)
  data<-cbind(data, sis1.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"sis1"

  sis2.names <-varnames[c(2,5,6,7)]
  sis2.names_scoring <-sis2.names
  for(i in 1:length(sis2.names_scoring)){
    sis2.names_scoring[[i]]<-paste("-", sis2.names_scoring[[i]], sep = "")
  }
  sis2.scores<-psych::scoreItems(sis2.names_scoring, dplyr::select(dat, !!sis2.names), totals = TRUE, min = 1, max = 4)
  data<-cbind(data, sis2.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"sis2"

  return(data)
}
