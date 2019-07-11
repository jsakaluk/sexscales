#' A function that scores Napper's Negative Impact of Hookups Inventory
#'
#' Calculates scale score scores
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "SSCS_1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreVanLankveld(dat, firstq = "SSCS_1")

scoreVanLankveld <- function(dat, firstq){

  itemnum<-12

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])
  embarrassment.names<- varnames[c(1,4,9,10,11,12)]
  selffocus.names<- varnames[c(2,3,5,6,7,8)]

  embarrassment.scores<-psych::scoreItems(embarrassment.names, dplyr::select(dat, !!embarrassment.names), totals = TRUE, min = 0, max = 4)
  data<-cbind(dat, embarrassment.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"embarrass"

  selffocus.scores<-psych::scoreItems(selffocus.names, dplyr::select(dat, !!selffocus.names), totals = TRUE, min = 0, max = 4)
  data<-cbind(data, selffocus.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"selffocus"

  return(data)
}
