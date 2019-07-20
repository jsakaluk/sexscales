#' A function that scores Humphreys's Sexual Consent Scale-Revised
#'
#' Calculates five subscale scores
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "SCSR_1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreHumphreysSCSR(dat, firstq = "SCSR_1")

scoreHumphreysSCSR <- function(dat, firstq){

  itemnum<-39

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])

  posatt.names<- varnames[1:11]
  posatt.scores<-psych::scoreItems(posatt.names, dplyr::select(dat, !!posatt.names), totals = FALSE, min = 1, max = 7)
  data<-cbind(dat, posatt.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"scsr.posatt"

  lackpbc.names<- varnames[12:22]
  lackpbc.scores<-psych::scoreItems(lackpbc.names, dplyr::select(dat, !!lackpbc.names), totals = FALSE, min = 1, max = 7)
  data<-cbind(data, lackpbc.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"scsr.lackpbc"

  norms.names<- varnames[23:29]
  norms.scores<-psych::scoreItems(norms.names, dplyr::select(dat, !!norms.names), totals = FALSE, min = 1, max = 7)
  data<-cbind(data, norms.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"scsr.norms"

  indirect.names<- varnames[30:35]
  indirect.scores<-psych::scoreItems(indirect.names, dplyr::select(dat, !!indirect.names), totals = FALSE, min = 1, max = 7)
  data<-cbind(data, indirect.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"scsr.indirect"

  awareness.names<- varnames[36:39]
  awareness.scores<-psych::scoreItems(awareness.names, dplyr::select(dat, !!awareness.names), totals = FALSE, min = 1, max = 7)
  data<-cbind(data,awareness.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"scsr.awareness"

  return(data)
}
