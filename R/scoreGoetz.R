#' A function that scores Goetz & Shackleford's Sexual Coercion in Intimate Relationships Scale
#'
#' Calculates three subscale and scale total scores
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "SCIRS_1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreGoetz(dat, firstq = "SCIRS_1")

scoreGoetz <- function(dat, firstq){

  itemnum<-34

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])

  resource.names<- varnames[c(1,2,3,4,5,6,9,10,11,17,22,23,31,32,33)]
  resource.scores<-psych::scoreItems(resource.names, dplyr::select(dat, !!resource.names), totals = TRUE, min = 0, max = 5)
  data<-cbind(dat, resource.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"scirs.resource"

  commitment.names<- varnames[c(7,8,12,15,18,19,20,21,30,34)]
  commitment.scores<-psych::scoreItems(commitment.names, dplyr::select(dat, !!commitment.names), totals = TRUE, min = 0, max = 5)
  data<-cbind(data, commitment.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"scirs.commitment"

  defection.names<- varnames[c(13,14,16,17,24,25,26,27,28,29)]
  defection.scores<-psych::scoreItems(defection.names, dplyr::select(dat, !!defection.names), totals = TRUE, min = 0, max = 5)
  data<-cbind(data, defection.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"scirs.defection"

  total.scores<-psych::scoreItems(varnames, dplyr::select(dat, !!varnames), totals = TRUE, min = 0, max = 5)
  data<-cbind(data, total.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"scirs.total"

  return(data)
}
