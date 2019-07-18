#' A function that scores Muehlenhard & Felts's Sexual Beliefs Scale
#'
#' Calculates five subscale scores
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "SBS_1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreMuehlenhardBeliefs(dat, firstq = "SBS_1")

scoreMuehlenhardBeliefs <- function(dat, firstq){

  itemnum<-40

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])

  tokenref.names<- varnames[c(13,20,28,36,7,17,24,39)]
  tokenref.scores<-psych::scoreItems(tokenref.names, dplyr::select(dat, !!tokenref.names), totals = FALSE, min = 0, max = 3)
  data<-cbind(dat, tokenref.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"tokenref"

  justforce.names<- varnames[c(11,23,29,33,3,8,19,31)]
  justforce.scores<-psych::scoreItems(justforce.names, dplyr::select(dat, !!justforce.names), totals = FALSE, min = 0, max = 3)
  data<-cbind(data, justforce.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"justforce"

  likeforce.names<- varnames[c(4,14,27,40,5,9,18,37)]
  likeforce.scores<-psych::scoreItems(likeforce.names, dplyr::select(dat, !!likeforce.names), totals = FALSE, min = 0, max = 3)
  data<-cbind(data, likeforce.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"likeforce"

  mendom.names<- varnames[c(1,10,26,30,12,16,22,35)]
  mendom.scores<-psych::scoreItems(mendom.names, dplyr::select(dat, !!mendom.names), totals = FALSE, min = 0, max = 3)
  data<-cbind(data, mendom.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"mendom"

  nomeansstop.names<- varnames[c(15,21,25,32,2,6,34,38)]
  nomeansstop.scores<-psych::scoreItems(nomeansstop.names, dplyr::select(dat, !!nomeansstop.names), totals = FALSE, min = 0, max = 3)
  data<-cbind(data, nomeansstop.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"nomeansstop"

  return(data)
}
