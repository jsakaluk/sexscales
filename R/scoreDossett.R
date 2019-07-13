#' A function that scores Dossett's Sexual Importance Scale
#'
#' Calculates scale score
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "SIS_1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreDosset(dat, firstq = "SIS_1")

scoreDosset <- function(dat, firstq){

  itemnum<-17

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])

  varnames_scoring<-varnames
  varnames_scoring[[15]]<-paste("-", varnames_scoring[[15]], sep = "")
  varnames_scoring[[17]]<-paste("-", varnames_scoring[[17]], sep = "")

  sis.scores<-psych::scoreItems(varnames_scoring, dplyr::select(dat, !!varnames), totals = TRUE, min = 1, max = 7)
  data<-cbind(dat, sis.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"SIS"

  return(data)
}
