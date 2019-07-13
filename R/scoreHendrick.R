#' A function that scores Hendrick & Hendrick's Brief Sexual Attitudes Scale
#'
#' Calculates gift, stigma, and process subscale scores
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "SAS_1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreHendrick(dat, firstq = "SAS_1")

scoreHendrick <- function(dat, firstq){

  itemnum<-23

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])
  permissive.names<- varnames[1:10]
  birthcont.names<- varnames[11:13]
  commun.names<- varnames[14:18]
  instrument.names<- varnames[19:23]

  perm.scores<-psych::scoreItems(permissive.names, dplyr::select(dat, !!permissive.names), totals = FALSE, min = 1, max = 5)
  data<-cbind(dat, perm.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"permissive"

  bc.scores<-psych::scoreItems(birthcont.names, dplyr::select(dat, !!birthcont.names), totals = FALSE, min = 1, max = 5)
  data<-cbind(data, bc.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"birthcont"

  commun.scores<-psych::scoreItems(commun.names, dplyr::select(dat, !!commun.names), totals = FALSE, min = 1, max = 5)
  data<-cbind(data, commun.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"communion"

  instrum.scores<-psych::scoreItems(instrument.names, dplyr::select(dat, !!instrument.names), totals = FALSE, min = 1, max = 5)
  data<-cbind(data, instrum.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"instrumentality"

  return(data)
}
