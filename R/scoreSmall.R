#' A function that scores Small's Perceived Costs and Benefits Scale
#'
#' Calculates costs and benefits subscale scores
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "APCBSSI_1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreSmall(dat, firstq = "APCBSSI_1")

scoreSmall <- function(dat, firstq){

  itemnum<-20

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])

  costs.names<-varnames[1:10]
  benefits.names<-varnames[11:20]

  cost.scores<-psych::scoreItems(costs.names, dplyr::select(dat, !!costs.names), totals = TRUE, min = 0, max = 3)
  data<-cbind(dat, cost.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"costs"

  benefit.scores<-psych::scoreItems(benefits.names, dplyr::select(dat, !!benefits.names), totals = TRUE, min = 0, max = 3)
  data<-cbind(data, benefit.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"benefits"

  return(data)
}
