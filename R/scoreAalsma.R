#' A function that scores Aalsma & Fortenberry's Childhood Sexual Abuse
#' Scale (CSAS)
#'
#' Calculates total CSAS scores based on a data frame. Users specify first column
#' with relevant data, and function seeks out the rest automatically.
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#' CSAS variable (e.g., "CSAS_1")
#' @return data frame with new "CSAS" vector of total scores
#' @export
#' @examples
#' dat = scoreAalsma(dat, firstq = "CSAS_1")

scoreAalsma <- function(dat, firstq){

  itemnum<-4

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])

  aalsma.scores<-psych::scoreItems(varnames, dplyr::select(dat, names(dat[firstcol]):names(dat[lastcol])), totals = TRUE, min = 0, max = 1)
  data<-cbind(dat, aalsma.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"CSAS"

  return(data)
}
