#' A function that scores Schwartz's First Coital Affective Reaction Scale
#'
#' Calculates scale score for for those with experience or anticipating their first experience
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "FCARS")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreSmall(dat, firstq = "FCARS")

scoreSchwartz <- function(dat, firstq){

  itemnum<-27

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])

  fcars.y.names<-varnames[2:14]
  fcars.y.names_scoring<- fcars.y.names
  fcars.y.names_scoring[[2]]<-paste("-", fcars.y.names_scoring[[2]], sep = "")
  fcars.y.names_scoring[[5]]<-paste("-", fcars.y.names_scoring[[5]], sep = "")
  fcars.y.names_scoring[[6]]<-paste("-", fcars.y.names_scoring[[6]], sep = "")
  fcars.y.names_scoring[[8]]<-paste("-", fcars.y.names_scoring[[8]], sep = "")
  fcars.y.names_scoring[[10]]<-paste("-", fcars.y.names_scoring[[10]], sep = "")
  fcars.y.names_scoring[[12]]<-paste("-", fcars.y.names_scoring[[12]], sep = "")
  fcars.y.scores<-psych::scoreItems(fcars.y.names_scoring, dplyr::select(dat, !!fcars.y.names), totals = TRUE, min = 1, max = 7)

  fcars.n.names<-varnames[15:27]
  fcars.n.names_scoring <-fcars.n.names
  fcars.n.names_scoring[[2]]<-paste("-", fcars.n.names_scoring[[2]], sep = "")
  fcars.n.names_scoring[[5]]<-paste("-", fcars.n.names_scoring[[5]], sep = "")
  fcars.n.names_scoring[[6]]<-paste("-", fcars.n.names_scoring[[6]], sep = "")
  fcars.n.names_scoring[[8]]<-paste("-", fcars.n.names_scoring[[8]], sep = "")
  fcars.n.names_scoring[[10]]<-paste("-", fcars.n.names_scoring[[10]], sep = "")
  fcars.n.names_scoring[[12]]<-paste("-", fcars.n.names_scoring[[12]], sep = "")
  fcars.n.scores<-psych::scoreItems(fcars.n.names_scoring, dplyr::select(dat, !!fcars.n.names), totals = TRUE, min = 1, max = 7)

  data<- dat
  data$fcars.total<- NA

  data$fcars.total[data$FCARS==1]<-fcars.y.scores$scores
  data$fcars.total[data$FCARS==2]<-fcars.n.scores$scores

  return(data)
}
