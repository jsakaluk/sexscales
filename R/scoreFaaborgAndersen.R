#' A function that scores Faaborg-Andersen et al.'s Index of Male Genital Image
#'
#' Calculates five subscale scores and total score
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "IMGI_1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreFaaborgAndersen(dat, firstq = "IMGI_1")

scoreFaaborgAndersen <- function(dat, firstq){

  itemnum<-14

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])

  appearance.names<- varnames[c(4,6,7,11)]
  appearance.scores<-psych::scoreItems(appearance.names, dplyr::select(dat, !!appearance.names), totals = TRUE, min = 1, max = 7)
  data<-cbind(dat, appearance.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"imgi_appearance"

  size.names<- varnames[c(1,2,3)]
  size.scores<-psych::scoreItems(size.names, dplyr::select(dat, !!size.names), totals = TRUE, min = 1, max = 7)
  data<-cbind(data, size.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"imgi_size"

  circumcision.names<- varnames[c(9)]
  data$imgi_circumcision <-data[names(data)==circumcision.names]
  names(data)[names(data)==names(data[ncol(data)])]<-"imgi_circumcision"

  ejaculatory.names<- varnames[c(10,13)]
  ejaculatory.scores<-psych::scoreItems(ejaculatory.names, dplyr::select(dat, !!ejaculatory.names), totals = TRUE, min = 1, max = 7)
  data<-cbind(data, ejaculatory.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"imgi_ejaculatory"

  pubichair.names<- varnames[c(12)]
  data$imgi_pubichair <-data[names(data)==pubichair.names]
  names(data)[names(data)==names(data[ncol(data)])]<-"imgi_circumcision"

  shape.names<- varnames[c(5,8,14)]
  shape.scores<-psych::scoreItems(shape.names, dplyr::select(dat, !!shape.names), totals = TRUE, min = 1, max = 7)
  data<-cbind(data, shape.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"imgi_shape"

  total<-psych::scoreItems(varnames, dplyr::select(dat, !!varnames), totals = TRUE, min = 1, max = 7)
  data<-cbind(data, total$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"imgi_total"

  return(data)
}
