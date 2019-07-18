#' A function that scores Liss et al's Enjoyment of Sexualization Score
#'
#' Calculates total score
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "ESS_1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreLiss(dat, firstq = "ESS_1")

scoreLiss <- function(dat, firstq){

  itemnum<-8

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])

  total.scores<-psych::scoreItems(varnames, dplyr::select(dat, !!varnames), totals = FALSE, min = 1, max = 6)
  data<-cbind(dat, total.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"ESS.total"

  return(data)
}
