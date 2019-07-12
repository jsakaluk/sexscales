#' A function that scores Hoon et al.'s Sexual Arousability Inventory
#'
#' Subscale sum scores
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "SAI_1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreHoon(dat, firstq = "SAI_1")

scoreHoon <- function(dat, firstq){

  itemnum<-56

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])

  arousability.names<-varnames[1:28]
  anxiety.names<-varnames[29:56]

  arousability.scores<-psych::scoreItems(arousability.names, dplyr::select(dat, !!arousability.names), totals = TRUE, min = -1, max = 5)
  data<-cbind(dat, arousability.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"arousability"

  anxiety.scores<-psych::scoreItems(anxiety.names, dplyr::select(dat, !!anxiety.names), totals = TRUE, min = -1, max = 5)
  data<-cbind(data, anxiety.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"anxiety"

  return(data)
}
