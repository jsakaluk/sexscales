#' A function that scores Jozkowski's Internal and External Consent Scale
#'
#' Calculates subscale scores for internal subscales (external TBD)
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "ICS_1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreJozkowski(dat, firstq = "ICS_1")

scoreJozkowski <- function(dat, firstq){

  itemnum<-26

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])

  physical.names<- varnames[c(2,8,12,17,22,24)]
  physical.scores<-psych::scoreItems(physical.names, dplyr::select(dat, !!physical.names), totals = FALSE, min = 1, max = 4)
  data<-cbind(dat,physical.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"ics_physical"

  safety.names<- varnames[c(4,5,15,16,20,21,23)]
  safety.scores<-psych::scoreItems(safety.names, dplyr::select(dat, !!safety.names), totals = FALSE, min = 1, max = 4)
  data<-cbind(data,safety.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"ics_safety"

  arousal.names<- varnames[c(1,3,6)]
  arousal.scores<-psych::scoreItems(arousal.names, dplyr::select(dat, !!arousal.names), totals = FALSE, min = 1, max = 4)
  data<-cbind(data,arousal.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"ics_arousal"

  agreement.names<- varnames[c(7,10,14,19,25)]
  agreement.scores<-psych::scoreItems(agreement.names, dplyr::select(dat, !!agreement.names), totals = FALSE, min = 1, max = 4)
  data<-cbind(data,agreement.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"ics_agreement"

  readiness.names<- varnames[c(9,11,13,18)]
  readiness.scores<-psych::scoreItems(readiness.names, dplyr::select(dat, !!readiness.names), totals = FALSE, min = 1, max = 4)
  data<-cbind(data,readiness.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"ics_readiness"

  return(data)
}
