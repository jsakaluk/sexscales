#' A function that scores Marelich et al.'s Sexual Deception Scale
#'
#' Calculates subscale and total scores
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "SDS_1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreMarelichSDS(dat, firstq = "SDS_1")

scoreMarelichSDS <- function(dat, firstq){

  itemnum<-15

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])

  lying.names<- varnames[c(1,2,9,11,12,13,15)]
  lying.scores<-psych::scoreItems(lying.names, dplyr::select(dat, !!lying.names), totals = FALSE, min = 0, max = 1)
  data<-cbind(dat,lying.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"sds.lying"

  selfserv.names<- varnames[c(4,7,8)]
  selfserv.scores<-psych::scoreItems(selfserv.names, dplyr::select(dat, !!selfserv.names), totals = FALSE, min = 0, max = 1)
  data<-cbind(data,selfserv.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"sds.selfserv"

  avoidconfr.names<- varnames[c(3,5,6,10,14)]
  avoidconfr.scores<-psych::scoreItems(avoidconfr.names, dplyr::select(dat, !!avoidconfr.names), totals = FALSE, min = 0, max = 1)
  data<-cbind(data,avoidconfr.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"sds.avoidconfr"

  total.scores<-psych::scoreItems(varnames, dplyr::select(dat, !!varnames), totals = FALSE, min = 0, max = 1)
  data<-cbind(data,total.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"sds.total"

  return(data)
}
