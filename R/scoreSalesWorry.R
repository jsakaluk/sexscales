#' A function that scores Sales et al.'s Worry About Sexual Outcome Scale
#'
#' Calculates pregnancy and STI/HIV subscales, as well as total worry
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "WASOS_1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreSalesWorry(dat, firstq = "WASOS_1")

scoreSalesWorry <- function(dat, firstq){

  itemnum<-10

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])
  stihiv.names<- varnames[1:8]
  pregnancy.names<- varnames[9:10]

  stihiv.scores<-psych::scoreItems(stihiv.names, dplyr::select(dat, !!stihiv.names), totals = TRUE, min = 1, max = 4)
  data<-cbind(dat, stihiv.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"worry_stihiv"

  pregnancy.scores<-psych::scoreItems(pregnancy.names, dplyr::select(dat, !!pregnancy.names), totals = TRUE, min = 1, max = 4)
  data<-cbind(data, pregnancy.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"worry_pregnancy"

  worry.scores<-psych::scoreItems(varnames, dplyr::select(dat, !!varnames), totals = TRUE, min = 1, max = 4)
  data<-cbind(data, worry.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"worry_total"

  return(data)
}
