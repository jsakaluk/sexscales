#' A function that scores Maxwell et al.'s Implicit Theories of Sexuality Scale
#'
#' Calculates growth and destiny subscale scores
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "ITSS_1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreMaxwell(dat, firstq = "ITSS_1")

scoreMaxwell <- function(dat, firstq){

  itemnum<-24

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])
  growth.names<- varnames[c(2,3,5,7,8,9,10,12,16,17,19,23,24)]
  destiny.names<- varnames[c(1,4,6,11,13,14,15,18,20,21,22)]

  growth.scores<-psych::scoreItems(growth.names, dplyr::select(dat, !!growth.names), totals = FALSE, min = 1, max = 7)
  data<-cbind(dat, growth.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"growth"

  destiny.scores<-psych::scoreItems(destiny.names, dplyr::select(dat, !!destiny.names), totals = FALSE, min = 1, max = 7)
  data<-cbind(data, destiny.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"destiny"

  return(data)
}
