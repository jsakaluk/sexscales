#' A function that scores Napper's Negative Impact of Hookups Inventory
#'
#' Calculates scale score scores
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "NIHI_1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreNapper(dat, firstq = "NIHI_1")

scoreNapper <- function(dat, firstq){

  itemnum<-14

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])

  nihi.scores<-psych::scoreItems(varnames, dplyr::select(dat, !!varnames), totals = TRUE, min = 0, max = 1)
  data<-cbind(dat, nihi.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"nihi.total"

  return(data)
}
