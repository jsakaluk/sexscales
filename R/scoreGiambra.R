#' A function that scores Giambra & Singer's Sexual Daydreaming Scale
#'
#' Calculates scale score
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "SDSIPI_1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreGiambra(dat, firstq = "SDSIPI_1")

scoreGiambra <- function(dat, firstq){

  itemnum<-12

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])

  sds.scores<-psych::scoreItems(varnames, dplyr::select(dat, !!varnames), totals = TRUE, min = 1, max = 5)
  data<-cbind(dat, sds.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"SDS"

  return(data)
}
