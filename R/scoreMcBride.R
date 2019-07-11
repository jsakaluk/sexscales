#' A function that scores McBride et al's Cognitive and Behavioral Outcomes of
#' Sexual Behavior Scale
#'
#' Calculates Cognitive and Behavioral subscale scores and total score
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "CBOSBS_1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreMcBride(dat, firstq = "CBOSBS_1")

scoreMcBride <- function(dat, firstq){

  itemnum<-36

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])

  cognitive.names<-varnames[1:20]
  behavior.names<-varnames[21:36]

  cognitive.scores<-psych::scoreItems(cognitive.names, dplyr::select(dat, !!cognitive.names), totals = TRUE, min = 0, max = 3)
  data<-cbind(dat, cognitive.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"CBOSBS.cognitive"

  behavior.scores<-psych::scoreItems(behavior.names, dplyr::select(dat, !!behavior.names), totals = TRUE, min = 0, max = 1)
  data<-cbind(data, behavior.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"CBOSBS.behavior"

  data$CBOSBS.total <-data$CBOSBS.cognitive + data$CBOSBS.behavior

  return(data)
}
