#' A function that scores Dillon & Worthington's Lesbian, Gay, and Bisexual Affirmative Counseling
#' Self-Efficacy Inventory
#'
#' Calculates five subscale scores and total score
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "LGBACSEI_1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreDillonCounseling(dat, firstq = "LGBACSEI_1")

scoreDillonCounseling <- function(dat, firstq){

  itemnum<-32

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])

  appknowledge.names<- varnames[1:13]
  appknowledge.scores<-psych::scoreItems(appknowledge.names, dplyr::select(dat, !!appknowledge.names), totals = TRUE, min = 1, max = 6)
  data<-cbind(dat, appknowledge.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"appknowledge"

  advocacy.names<- varnames[19:25]
  advocacy.scores<-psych::scoreItems(advocacy.names, dplyr::select(dat, !!advocacy.names), totals = TRUE, min = 1, max = 6)
  data<-cbind(data, advocacy.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"advocacy"

  awareness.names<- varnames[14:18]
  awareness.scores<-psych::scoreItems(awareness.names, dplyr::select(dat, !!awareness.names), totals = TRUE, min = 1, max = 6)
  data<-cbind(data, awareness.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"awareness"

  assessment.names<- varnames[26:29]
  assessment.scores<-psych::scoreItems(assessment.names, dplyr::select(dat, !!assessment.names), totals = TRUE, min = 1, max = 6)
  data<-cbind(data, assessment.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"assessment"

  relationship.names<- varnames[30:32]
  relationship.scores<-psych::scoreItems(relationship.names, dplyr::select(dat, !!relationship.names), totals = TRUE, min = 1, max = 6)
  data<-cbind(data, relationship.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"relationship"

  total<-psych::scoreItems(varnames, dplyr::select(dat, !!varnames), totals = TRUE, min = 1, max = 6)
  data<-cbind(data, total$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"lgbcsi_total"

  return(data)
}
