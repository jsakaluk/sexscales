#' A function that scores Eriksson & Humphrey's Virginity Beliefs Scale
#'
#' Calculates gift, stigma, and process subscale scores
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "VBS_1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreEriksson(dat, firstq = "VBS_1")

scoreEriksson <- function(dat, firstq){

  itemnum<-22

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])
  gift.names<- varnames[c(2,3,5,7,10,12,14,16,18,20)]
  stigma.names<- varnames[c(1,6,8,11,15,17,19,21)]
  process.names<- varnames[c(4,9,13,22)]

  gift.scores<-psych::scoreItems(gift.names, dplyr::select(dat, !!gift.names), totals = FALSE, min = 1, max = 7)
  data<-cbind(dat, gift.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"gift"

  stigma.scores<-psych::scoreItems(stigma.names, dplyr::select(dat, !!stigma.names), totals = FALSE, min = 1, max = 7)
  data<-cbind(data, stigma.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"stigma"

  process.scores<-psych::scoreItems(process.names, dplyr::select(dat, !!process.names), totals = FALSE, min = 1, max = 7)
  data<-cbind(data, process.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"process"

  return(data)
}
