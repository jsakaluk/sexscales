#' A function that scores Miller & Byers's Sexual Intervention Self-Efficacy Scale
#' Self-Efficacy Inventory
#'
#' Calculates three subscale scores
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "SIEF_1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreMiller(dat, firstq = "SIEF_1")

scoreMiller <- function(dat, firstq){

  itemnum<-19

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])

  relayinfo.names<- varnames[13:19]
  relayinfo.scores<-psych::scoreItems(relayinfo.names, dplyr::select(dat, !!relayinfo.names), totals = TRUE, min = 1, max = 6)
  data<-cbind(dat, relayinfo.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"relayinfo"

  skills.names<- varnames[c(1,3,5,6,8,10,12)]
  skills.names_scoring <-skills.names
  skills.names_scoring[[1]]<-paste("-", skills.names_scoring[[1]], sep = "")
  skills.names_scoring[[2]]<-paste("-", skills.names_scoring[[2]], sep = "")
  skills.names_scoring[[5]]<-paste("-", skills.names_scoring[[5]], sep = "")
  skills.scores<-psych::scoreItems(skills.names_scoring, dplyr::select(dat, !!skills.names), totals = TRUE, min = 1, max = 6)
  data<-cbind(data, skills.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"skills"

  comfort.names<- varnames[c(2,4,7,9,11)]
  comfort.names_scoring <-comfort.names
  comfort.names_scoring[[1]]<-paste("-", comfort.names_scoring[[1]], sep = "")
  comfort.names_scoring[[4]]<-paste("-", comfort.names_scoring[[4]], sep = "")
  comfort.names_scoring[[5]]<-paste("-", comfort.names_scoring[[5]], sep = "")
  comfort.scores<-psych::scoreItems(comfort.names_scoring, dplyr::select(dat, !!comfort.names), totals = TRUE, min = 1, max = 6)
  data<-cbind(data, comfort.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"comfort"

  return(data)
}
