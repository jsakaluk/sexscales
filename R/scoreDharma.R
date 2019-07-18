#' A function that scores Dharma et al's Trans-Specific Body Image Worries Scale
#'
#' Calculates total score
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "TSSBIW_1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreDharma(dat, firstq = "TSSBIW_1")

scoreDharma <- function(dat, firstq){

  itemnum<-5

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])

  total.scores<-psych::scoreItems(varnames, dplyr::select(dat, !!varnames), totals = TRUE, min = 0, max = 4)
  data<-cbind(dat, total.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"worries"

  return(data)
}
