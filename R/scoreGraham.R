#' A function that scores Graham et al.'s Sexual Excitation/Sexual Inhibition Inventory for
#' Women
#'
#' Subscale average scores and higher order average scores
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "SAI_1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreGraham(dat, firstq = "SESIW1")

scoreGraham <- function(dat, firstq){

  itemnum<-36

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])

  arousability.names<-varnames[c(15,17,19,20,24,25,26,30,32)]
  arousability.scores<-psych::scoreItems(arousability.names, dplyr::select(dat, !!arousability.names), totals = FALSE, min = 1, max = 4)
  data<-cbind(dat, arousability.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"sesiw.arous"

  partnerchar.names<-varnames[c(5,8,10,12)]
  partnerchar.scores<-psych::scoreItems(partnerchar.names, dplyr::select(dat, !!partnerchar.names), totals = FALSE, min = 1, max = 4)
  data<-cbind(data, partnerchar.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"sesiw.partnerchar"

  sexpower.names<-varnames[c(2,6,27,28)]
  sexpower.names_scoring<-sexpower.names
  sexpower.names_scoring[[3]]<-paste("-", sexpower.names_scoring[[3]], sep = "")
  sexpower.scores<-psych::scoreItems(sexpower.names_scoring, dplyr::select(dat, !!sexpower.names), totals = FALSE, min = 1, max = 4)
  data<-cbind(data, sexpower.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"sesiw.sexpower"

  smell.names<-varnames[c(22,23)]
  smell.scores<-psych::scoreItems(smell.names, dplyr::select(dat, !!smell.names), totals = FALSE, min = 1, max = 4)
  data<-cbind(data, smell.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"sesiw.smell"

  setting.name<-varnames[c(3,4,7,13)]
  setting.name_scoring<-setting.name
  setting.name_scoring[[2]]<-paste("-", setting.name_scoring[[2]], sep = "")
  setting.name_scoring[[3]]<-paste("-", setting.name_scoring[[3]], sep = "")
  setting.scores<-psych::scoreItems(setting.name_scoring, dplyr::select(dat, !!setting.name), totals = FALSE, min = 1, max = 4)
  data<-cbind(data, setting.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"sesiw.setting"

  sexfuncconc.name<-varnames[c(9,18,29,31)]
  sexfuncconc.scores<-psych::scoreItems(sexfuncconc.name, dplyr::select(dat, !!sexfuncconc.name), totals = FALSE, min = 1, max = 4)
  data<-cbind(data, sexfuncconc.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"sesiw.sexfuncconc"

  arouscontin.name<-varnames[c(34,35,36)]
  arouscontin.scores<-psych::scoreItems(arouscontin.name, dplyr::select(dat, !!arouscontin.name), totals = FALSE, min = 1, max = 4)
  data<-cbind(data, arouscontin.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"sesiw.arouscontin"

  relimpt.name<-varnames[c(1,11,14,16,21,33)]
  relimpt.scores<-psych::scoreItems(relimpt.name, dplyr::select(dat, !!relimpt.name), totals = FALSE, min = 1, max = 4)
  data<-cbind(data, relimpt.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"sesiw.relimpt"

  data$sesiw.excit<-(data$sesiw.arous+data$sesiw.partnerchar+data$sesiw.sexpower+data$sesiw.smell+data$sesiw.setting)/5
  data$sesiw.inhib<-(data$sesiw.sexfuncconc+data$sesiw.arouscontin+data$sesiw.relimpt)/3

  return(data)
}
