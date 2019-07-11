#' A function that scores Berglas et al.'s Sexual Relationship Rights (SRR) measure
#'
#' Subscale and total average scores
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "Q2_1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreBerglas(dat, firstq = "AA_SSR_1")

scoreBerglas <- function(dat, firstq){

  itemnum<-10

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])

  refusal.names<-varnames[1:5]
  engagement.names<-varnames[6:10]

  refusal.scores<-psych::scoreItems(refusal.names, dplyr::select(dat, !!refusal.names), totals = FALSE, min = 1, max = 4)
  data<-cbind(dat, refusal.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"srr.refusal"

  engagement.scores<-psych::scoreItems(engagement.names, dplyr::select(dat, !!engagement.names), totals = FALSE, min = 1, max = 4)
  data<-cbind(data, engagement.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"srr.engagement"

  total.scores<-psych::scoreItems(varnames, dplyr::select(dat, !!varnames), totals = FALSE, min = 1, max = 4)
  data<-cbind(data, total.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"srr.total"

  return(data)
}
