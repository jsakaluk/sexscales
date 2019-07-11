#' A function that scores Lottes et al.'s Sexual Socialization Instrument (SSI) measure
#'
#' Subscale and total average scores
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "SSI_1")
#' @return data frame with new vectors of subscale and overall scores
#' @export
#' @examples
#' dat = scoreLottesSSI(dat, firstq = "SSI_1")

scoreLottesSSI <- function(dat, firstq){

  itemnum<-20

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])

  parental.names<-varnames[c(1,3,6,9,12,16,19,20)]
  parental.names_scoring<-parental.names
  parental.names_scoring[[1]]<-paste("-", parental.names_scoring[[1]], sep = "")
  parental.names_scoring[[3]]<-paste("-", parental.names_scoring[[3]], sep = "")
  parental.names_scoring[[6]]<-paste("-", parental.names_scoring[[6]], sep = "")
  parental.names_scoring[[7]]<-paste("-", parental.names_scoring[[7]], sep = "")

  peer.names<-varnames[c(2,4,5,7,8,10,11,13,14,15,17,18)]
  peer.names_scoring<-peer.names
  peer.names_scoring[[2]]<-paste("-", peer.names_scoring[[2]], sep = "")
  peer.names_scoring[[5]]<-paste("-", peer.names_scoring[[5]], sep = "")
  peer.names_scoring[[7]]<-paste("-", peer.names_scoring[[7]], sep = "")
  peer.names_scoring[[8]]<-paste("-", peer.names_scoring[[8]], sep = "")
  peer.names_scoring[[9]]<-paste("-", peer.names_scoring[[9]], sep = "")
  peer.names_scoring[[10]]<-paste("-", peer.names_scoring[[10]], sep = "")
  peer.names_scoring[[12]]<-paste("-", peer.names_scoring[[12]], sep = "")

  parental.scores<-psych::scoreItems(parental.names_scoring, dplyr::select(dat, !!parental.names), totals = TRUE, min = 1, max = 5)
  data<-cbind(dat, parental.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"ssi.parental"

  peer.scores<-psych::scoreItems(peer.names_scoring, dplyr::select(dat, !!peer.names), totals = TRUE, min = 1, max = 5)
  data<-cbind(data, peer.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"ssi.peer"

  data$ssi.total<-data$ssi.parental+data$ssi.peer

  return(data)
}
