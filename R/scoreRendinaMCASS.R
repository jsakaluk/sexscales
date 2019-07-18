#' A function that scores Rendina et al.'s Maladaptive Cognitions About Sex Scale
#'
#' Calculates positive and negative total scores, as well as subscales for dominance and submission
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "MCASS_1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreRendinaMCASS(dat, firstq = "MCASS_1")

scoreRendinaMCASS <- function(dat, firstq){

  itemnum<-11

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])

  necessity.names<- varnames[1:5]
  necessity.scores<-psych::scoreItems(necessity.names, dplyr::select(dat, !!necessity.names), totals = FALSE, min = 1, max = 5)
  data<-cbind(dat, necessity.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"necessity"

  disqbenefits.names<- varnames[6:8]
  disqbenefits.scores<-psych::scoreItems(disqbenefits.names, dplyr::select(dat, !!disqbenefits.names), totals = FALSE, min = 1, max = 5)
  data<-cbind(data, disqbenefits.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"disqbenefits"

  efficacy.names<- varnames[9:11]
  efficacy.scores<-psych::scoreItems(efficacy.names, dplyr::select(dat, !!efficacy.names), totals = FALSE, min = 1, max = 5)
  data<-cbind(data, efficacy.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"efficacy"

  return(data)
}
