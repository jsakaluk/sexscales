#' A function that scores Visser et al.'s Male Enjoyment of Sexualization Scale
#'
#' Calculates total score
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "MESS_1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreVisser(dat, firstq = "MESS_1")

scoreVisser <- function(dat, firstq){

  itemnum<-8

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])

  total.scores<-psych::scoreItems(varnames, dplyr::select(dat, !!varnames), totals = FALSE, min = 1, max = 5)
  data<-cbind(dat, total.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"ESS_M"

  return(data)
}
