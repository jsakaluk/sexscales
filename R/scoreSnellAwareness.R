#' A function that scores Snell et al.'s Sexual Awareness Questionnaire
#'
#' Calculates positive and negative total scores, as well as subscales for dominance and submission
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "SAS_1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreSnellAwareness(dat, firstq = "SAS_1")

scoreSnellAwareness <- function(dat, firstq){

  itemnum<-36

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])

  consciousness.names<- varnames[c(1,4,10,13,22,25)]
  consciousness.scores<-psych::scoreItems(consciousness.names, dplyr::select(dat, !!consciousness.names), totals = TRUE, min = 0, max = 4)
  data<-cbind(dat, consciousness.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"consciousness"

  monitoring.names<- varnames[c(2,5,14,17,23,26,28,31,32)]
  monitoring.names_scoring<-monitoring.names
  monitoring.names_scoring[[5]]<-paste("-", monitoring.names_scoring[[5]], sep = "")
  monitoring.names_scoring[[8]]<-paste("-", monitoring.names_scoring[[8]], sep = "")
  monitoring.names_scoring[[9]]<-paste("-", monitoring.names_scoring[[9]], sep = "")
  monitoring.scores<-psych::scoreItems(monitoring.names_scoring, dplyr::select(dat, !!monitoring.names), totals = TRUE, min = 0, max = 4)
  data<-cbind(data, monitoring.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"monitoring"

  appeal.names<- varnames[c(8,11,29)]
  appeal.scores<-psych::scoreItems(appeal.names, dplyr::select(dat, !!appeal.names), totals = TRUE, min = 0, max = 4)
  data<-cbind(data, appeal.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"appeal"

  assertive.names<- varnames[c(3,6,9,12,15,18,24)]
  assertive.names_scoring<-assertive.names
  assertive.names_scoring[[2]]<-paste("-", assertive.names_scoring[[2]], sep = "")
  assertive.names_scoring[[3]]<-paste("-", assertive.names_scoring[[3]], sep = "")
  assertive.scores<-psych::scoreItems(assertive.names_scoring, dplyr::select(dat, !!assertive.names), totals = TRUE, min = 0, max = 4)
  data<-cbind(data, assertive.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"assertive"

  return(data)
}
