#' A function that scores Camilleri's Tactics to Obtain Sex Scale
#'
#' Calculates coerce, coax, and total scores
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "TOSS_1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreCamilleri(dat, firstq = "TOSS_1")

scoreCamilleri <- function(dat, firstq){

  itemnum<-62

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])

  coerce.names<- varnames[c(2,3,5,6,8,9,11,12,13,16,17,18,23,24,26,27,28,29,31,
                            33,34,36,37,39,40,42,43,44,47,48,49,54,55,57,58,59,60,62)]
  coerce.scores<-psych::scoreItems(coerce.names, dplyr::select(dat, !!coerce.names), totals = TRUE, min = 0, max = 4)
  data<-cbind(dat, coerce.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"toss.coerce"

  coax.names<- varnames[c(1,4,7,10,14,15,19,20,21,22,25,30,
                          32,35,38,41,45,46,50,51,52,53,56,61)]
  coax.scores<-psych::scoreItems(coax.names, dplyr::select(dat, !!coax.names), totals = TRUE, min = 0, max = 4)
  data<-cbind(data, coax.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"toss.coax"

  total.scores<-psych::scoreItems(varnames, dplyr::select(dat, !!varnames), totals = TRUE, min = 0, max = 4)
  data<-cbind(data, total.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"toss.total"

  return(data)
}
