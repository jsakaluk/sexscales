#' A function that scores Renaud & Byers Sexual Cognition Checklist
#'
#' Calculates positive and negative total scores, as well as subscales for dominance and submission
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "SCC_P_1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreRenaud(dat, firstq = "SCC_P_1")

scoreRenaud <- function(dat, firstq){

  itemnum<-114

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])

  poscog.names<- varnames[1:56]
  poscog.scores<-psych::scoreItems(poscog.names, dplyr::select(dat, !!poscog.names), totals = TRUE, min = 0, max = 6)
  data<-cbind(dat, poscog.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"poscog"

  negcog.names<- varnames[58:113]
  negcog.scores<-psych::scoreItems(negcog.names, dplyr::select(dat, !!negcog.names), totals = TRUE, min = 0, max = 6)
  data<-cbind(data, negcog.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"negcog"

  posdom.names<- poscog.names[c(11,22,27,30,39,48)]
  posdom.scores<-psych::scoreItems(posdom.names, dplyr::select(dat, !!posdom.names), totals = TRUE, min = 0, max = 6)
  data<-cbind(data, posdom.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"posdom"

  negdom.names<- negcog.names[c(11,22,27,30,39,48)]
  negdom.scores<-psych::scoreItems(negdom.names, dplyr::select(dat, !!negdom.names), totals = TRUE, min = 0, max = 6)
  data<-cbind(data, negdom.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"negdom"

  possub.names<- poscog.names[c(5,6,10,19,20,23,26,31,34,47)]
  possub.scores<-psych::scoreItems(possub.names, dplyr::select(dat, !!possub.names), totals = TRUE, min = 0, max = 6)
  data<-cbind(data, possub.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"possub"

  negsub.names<- negcog.names[c(5,6,10,19,20,23,26,31,34,47)]
  negsub.scores<-psych::scoreItems(negsub.names, dplyr::select(dat, !!negsub.names), totals = TRUE, min = 0, max = 6)
  data<-cbind(data, negsub.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"negsub"

  return(data)
}
