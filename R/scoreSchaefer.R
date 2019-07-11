#' A function that scores Schaefer et al.'s Empathy for Children Scale
#'
#' Calculates scenario-, subscale-, and total-scores based on a data frame. Users specify first column
#' with relevant data, and function seeks out the rest automatically.
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "Q2_1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreSchaefer(dat, firstq = "Q2_1")

scoreSchaefer <- function(dat, firstq){

  itemnum<-82

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum
  varnames<-names(dat[firstcol:lastcol])

  s1.cog.names<-varnames[1:15]
  s1.cog.names_scoring<-varnames[1:15]
  s1.cog.names_scoring[[4]]<-paste("-", s1.cog.names_scoring[[4]], sep = "")
  s1.cog.names_scoring[[7]]<-paste("-", s1.cog.names_scoring[[7]], sep = "")

  s1.emo.names<-varnames[16:25]
  s1.emo.names_scoring<-varnames[16:25]
  s1.emo.names_scoring[[1]]<-paste("-", s1.emo.names_scoring[[1]], sep = "")
  s1.emo.names_scoring[[8]]<-paste("-", s1.emo.names_scoring[[8]], sep = "")
  s1.emo.names_scoring[[9]]<-paste("-", s1.emo.names_scoring[[9]], sep = "")

  s2.cog.names<-varnames[28:42]
  s2.cog.names_scoring<-varnames[28:42]
  s2.cog.names_scoring[[4]]<-paste("-", s2.cog.names_scoring[[4]], sep = "")
  s2.cog.names_scoring[[7]]<-paste("-", s2.cog.names_scoring[[7]], sep = "")

  s2.emo.names<-varnames[43:52]
  s2.emo.names_scoring<-varnames[43:52]
  s2.emo.names_scoring[[1]]<-paste("-", s2.emo.names_scoring[[1]], sep = "")
  s2.emo.names_scoring[[8]]<-paste("-", s2.emo.names_scoring[[8]], sep = "")
  s2.emo.names_scoring[[9]]<-paste("-", s2.emo.names_scoring[[9]], sep = "")

  s3.cog.names<-varnames[55:69]
  s3.cog.names_scoring<-varnames[55:69]
  s3.cog.names_scoring[[4]]<-paste("-", s3.cog.names_scoring[[4]], sep = "")
  s3.cog.names_scoring[[7]]<-paste("-", s3.cog.names_scoring[[7]], sep = "")

  s3.emo.names<-varnames[71:80]
  s3.emo.names_scoring<-varnames[71:80]
  s3.emo.names_scoring[[1]]<-paste("-", s3.emo.names_scoring[[1]], sep = "")
  s3.emo.names_scoring[[8]]<-paste("-", s3.emo.names_scoring[[8]], sep = "")
  s3.emo.names_scoring[[9]]<-paste("-", s3.emo.names_scoring[[9]], sep = "")

  s1.cog.scores<-psych::scoreItems(s1.cog.names_scoring, dplyr::select(dat, !!s1.cog.names), totals = TRUE, min = 0, max = 4)
  data<-cbind(dat, s1.cog.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"s1.cog"

  s2.cog.scores<-psych::scoreItems(s2.cog.names_scoring, dplyr::select(dat, !!s2.cog.names), totals = TRUE, min = 0, max = 4)
  data<-cbind(data, s2.cog.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"s2.cog"

  s3.cog.scores<-psych::scoreItems(s3.cog.names_scoring, dplyr::select(dat, !!s3.cog.names), totals = TRUE, min = 0, max = 4)
  data<-cbind(data, s3.cog.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"s3.cog"

  s1.emo.scores<-psych::scoreItems(s1.emo.names_scoring, dplyr::select(dat, !!s1.emo.names), totals = TRUE, min = 0, max = 4)
  data<-cbind(data, s1.emo.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"s1.emo"

  s2.emo.scores<-psych::scoreItems(s2.emo.names_scoring, dplyr::select(dat, !!s2.emo.names), totals = TRUE, min = 0, max = 4)
  data<-cbind(data, s2.emo.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"s2.emo"

  s3.emo.scores<-psych::scoreItems(s3.emo.names_scoring, dplyr::select(dat, !!s3.emo.names), totals = TRUE, min = 0, max = 4)
  data<-cbind(data, s3.emo.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"s3.emo"

  data<-dplyr::mutate(data, s1.total = s1.cog + s1.emo,
                      s2.total = s2.cog + s2.emo,
                      s3.total = s3.cog + s3.emo,
                      cog.total = s1.cog + s2.cog + s3.cog,
                      emo.total = s1.emo + s2.emo + s3.emo)

  return(data)
}
