#' A function that scores Pascoal et al.'s Beliefs About Sexual Function Scale
#'
#' Calculates five subscale scores and total score
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "BASEF_1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scorePascoal(dat, firstq = "BASEF_1")

scorePascoal <- function(dat, firstq){

  itemnum<-15

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])

  analbeliefs.names<- varnames[c(1,7,14)]
  analbeliefs.scores<-psych::scoreItems(analbeliefs.names, dplyr::select(dat, !!analbeliefs.names), totals = TRUE, min = 1, max = 5)
  data<-cbind(dat, analbeliefs.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"anal"

  maleperfbeliefs.names<- varnames[c(3,5,13)]
  maleperfbeliefs.scores<-psych::scoreItems(maleperfbeliefs.names, dplyr::select(dat, !!maleperfbeliefs.names), totals = TRUE, min = 1, max = 5)
  data<-cbind(data, maleperfbeliefs.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"maleperf"

  aging.names<- varnames[c(2,8,11)]
  aging.scores<-psych::scoreItems(aging.names, dplyr::select(dat, !!aging.names), totals = TRUE, min = 1, max = 5)
  data<-cbind(data, aging.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"aging"

  sexpain.names<- varnames[c(4,6,15)]
  sexpain.scores<-psych::scoreItems(sexpain.names, dplyr::select(dat, !!sexpain.names), totals = TRUE, min = 1, max = 5)
  data<-cbind(data, sexpain.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"sexpain"

  primrel.names<- varnames[c(9,10,12)]
  primrel.scores<-psych::scoreItems(primrel.names, dplyr::select(dat, !!primrel.names), totals = TRUE, min = 1, max = 5)
  data<-cbind(data, primrel.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"primrel"

  total<-psych::scoreItems(varnames, dplyr::select(dat, !!varnames), totals = TRUE, min = 1, max = 5)
  data<-cbind(data, total$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"total"

  return(data)
}
