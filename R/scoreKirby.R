#' A function that scores Kirby's Mathtech Questionnaire
#'
#' Calculate's knowledge, attitude subscale scores.
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "Q2_1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreKirby(dat, firstq = "Q2_1")
scoreKirby <- function(dat, firstq){

  itemnum<-147

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum
  varnames<-names(dat[firstcol:lastcol])

  knowledge.names<-varnames[1:34]
  knowledge.key <-c(2,2,4,5,4,1,1,5,5,1,3,
                    5,1,3,4,5,4,4,1,2,1,5,
                    1,4,3,5,1,2,2,5,5,2,5,3)

  knowledge.score<-psych::score.multiple.choice(knowledge.key,dplyr::select(dat, !!knowledge.names),totals = T, short = F)
  data<-cbind(dat, knowledge.score$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"knowledge"

  attitude.names<-varnames[35:104]

  ltg.names<-c(attitude.names[10], attitude.names[23], attitude.names[30], attitude.names[37], attitude.names[51])
  ltg.names_scoring<-ltg.names
  ltg.names_scoring[[1]]<-paste("-", ltg.names_scoring[[1]], sep = "")
  ltg.scores<-psych::scoreItems(ltg.names_scoring, dplyr::select(dat, !!ltg.names), totals = FALSE, min = 1, max = 5)
  data<-cbind(data, ltg.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"a_ltg"


  psv.names<-c(attitude.names[5],attitude.names[13],attitude.names[25],attitude.names[49],attitude.names[70])
  psv.names_scoring<-  psv.names
  psv.names_scoring[[1]]<-paste("-", psv.names_scoring[[1]], sep = "")
  psv.names_scoring[[2]]<-paste("-", psv.names_scoring[[2]], sep = "")
  psv.names_scoring[[3]]<-paste("-", psv.names_scoring[[3]], sep = "")
  psv.scores<-psych::scoreItems(psv.names_scoring, dplyr::select(dat, !!psv.names), totals = FALSE, min = 1, max = 5)
  data<-cbind(data, psv.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"a_psv"

  uen.names<-c(attitude.names[14], attitude.names[17], attitude.names[48], attitude.names[56], attitude.names[62])
  uen.names_scoring<-uen.names
  uen.names_scoring[[5]]<-paste("-", uen.names_scoring[[5]], sep = "")
  uen.scores<-psych::scoreItems(uen.names_scoring, dplyr::select(dat, !!uen.names), totals = FALSE, min = 1, max = 5)
  data<-cbind(data, uen.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"a_uen"

  upsb.names<-c(attitude.names[6], attitude.names[19], attitude.names[27], attitude.names[34], attitude.names[66])
  upsb.names_scoring<-upsb.names
  upsb.names_scoring[[1]]<-paste("-", upsb.names_scoring[[1]], sep = "")
  upsb.names_scoring[[4]]<-paste("-", upsb.names_scoring[[4]], sep = "")
  upsb.scores<-psych::scoreItems(upsb.names_scoring, dplyr::select(dat, !!upsb.names), totals = FALSE, min = 1, max = 5)
  data<-cbind(data, upsb.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"a_upsb"

  upsr.names<-c(attitude.names[21], attitude.names[31], attitude.names[36], attitude.names[45], attitude.names[52])
  upsr.names_scoring<-upsr.names
  upsr.names_scoring[[1]]<-paste("-", upsr.names_scoring[[1]], sep = "")
  upsr.names_scoring[[4]]<-paste("-", upsr.names_scoring[[4]], sep = "")
  upsr.names_scoring[[5]]<-paste("-", upsr.names_scoring[[5]], sep = "")
  upsr.scores<-psych::scoreItems(upsr.names_scoring, dplyr::select(dat, !!upsr.names), totals = FALSE, min = 1, max = 5)
  data<-cbind(data, upsr.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"a_upsr"

  grb.names<-c(attitude.names[8], attitude.names[28], attitude.names[41], attitude.names[50], attitude.names[65])
  grb.names_scoring<-grb.names
  grb.names_scoring[[1]]<-paste("-", grb.names_scoring[[1]], sep = "")
  grb.names_scoring[[2]]<-paste("-", grb.names_scoring[[2]], sep = "")
  grb.scores<-psych::scoreItems(grb.names_scoring, dplyr::select(dat, !!grb.names), totals = FALSE, min = 1, max = 5)
  data<-cbind(data, grb.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"a_grb"

  sil.names<-c(attitude.names[12], attitude.names[42], attitude.names[55], attitude.names[58], attitude.names[64])
  sil.names_scoring<-sil.names
  sil.names_scoring[[1]]<-paste("-", sil.names_scoring[[1]], sep = "")
  sil.names_scoring[[2]]<-paste("-", sil.names_scoring[[2]], sep = "")
  sil.names_scoring[[4]]<-paste("-", sil.names_scoring[[4]], sep = "")
  sil.scores<-psych::scoreItems(sil.names_scoring, dplyr::select(dat, !!sil.names), totals = FALSE, min = 1, max = 5)
  data<-cbind(data, sil.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"a_sil"

  ibc.names<-c(attitude.names[4], attitude.names[16], attitude.names[40], attitude.names[59], attitude.names[61])
  ibc.names_scoring<-ibc.names
  ibc.names_scoring[[2]]<-paste("-", ibc.names_scoring[[2]], sep = "")
  ibc.names_scoring[[4]]<-paste("-", ibc.names_scoring[[4]], sep = "")
  ibc.scores<-psych::scoreItems(ibc.names_scoring, dplyr::select(dat, !!ibc.names), totals = FALSE, min = 1, max = 5)
  data<-cbind(data, ibc.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"a_ibc"

  pi.names<-c(attitude.names[2], attitude.names[20], attitude.names[22], attitude.names[29], attitude.names[63])
  pi.names_scoring<-pi.names
  pi.names_scoring[[3]]<-paste("-", pi.names_scoring[[3]], sep = "")
  pi.names_scoring[[5]]<-paste("-", pi.names_scoring[[5]], sep = "")
  pi.scores<-psych::scoreItems(pi.names_scoring, dplyr::select(dat, !!pi.names), totals = FALSE, min = 1, max = 5)
  data<-cbind(data, pi.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"a_pi"

  pfsa.names<-c(attitude.names[9], attitude.names[15], attitude.names[46], attitude.names[47], attitude.names[54])
  pfsa.names_scoring<-pfsa.names
  pfsa.names_scoring[[1]]<-paste("-", pfsa.names_scoring[[1]], sep = "")
  pfsa.names_scoring[[3]]<-paste("-", pfsa.names_scoring[[3]], sep = "")
  pfsa.scores<-psych::scoreItems(pfsa.names_scoring, dplyr::select(dat, !!pfsa.names), totals = FALSE, min = 1, max = 5)
  data<-cbind(data, pfsa.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"a_pfsa"

  rif.names<-c(attitude.names[11], attitude.names[24], attitude.names[53], attitude.names[60], attitude.names[69])
  rif.names_scoring<-rif.names
  rif.names_scoring[[1]]<-paste("-", rif.names_scoring[[1]], sep = "")
  rif.names_scoring[[2]]<-paste("-", rif.names_scoring[[2]], sep = "")
  rif.names_scoring[[4]]<-paste("-", rif.names_scoring[[4]], sep = "")
  rif.scores<-psych::scoreItems(rif.names_scoring, dplyr::select(dat, !!rif.names), totals = FALSE, min = 1, max = 5)
  data<-cbind(data, rif.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"a_rif"

  se.names<-c(attitude.names[3], attitude.names[26], attitude.names[35], attitude.names[44], attitude.names[68])
  se.names_scoring<-se.names
  se.names_scoring[[2]]<-paste("-", se.names_scoring[[2]], sep = "")
  se.names_scoring[[3]]<-paste("-", se.names_scoring[[3]], sep = "")
  se.names_scoring[[5]]<-paste("-", se.names_scoring[[5]], sep = "")
  se.scores<-psych::scoreItems(se.names_scoring, dplyr::select(dat, !!se.names), totals = FALSE, min = 1, max = 5)
  data<-cbind(data, se.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"a_se"

  sps.names<-c(attitude.names[7], attitude.names[18], attitude.names[33], attitude.names[39], attitude.names[57])
  sps.names_scoring<-sps.names
  sps.names_scoring[[1]]<-paste("-", sps.names_scoring[[1]], sep = "")
  sps.names_scoring[[2]]<-paste("-", sps.names_scoring[[2]], sep = "")
  sps.names_scoring[[4]]<-paste("-", sps.names_scoring[[4]], sep = "")
  sps.scores<-psych::scoreItems(sps.names_scoring, dplyr::select(dat, !!sps.names), totals = FALSE, min = 1, max = 5)
  data<-cbind(data, sps.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"a_sps"

  ssr.names<-c(attitude.names[1], attitude.names[32], attitude.names[38], attitude.names[43], attitude.names[67])
  ssr.names_scoring<-ssr.names
  ssr.names_scoring[[2]]<-paste("-", ssr.names_scoring[[2]], sep = "")
  ssr.names_scoring[[3]]<-paste("-", ssr.names_scoring[[3]], sep = "")
  ssr.names_scoring[[4]]<-paste("-", ssr.names_scoring[[4]], sep = "")
  ssr.scores<-psych::scoreItems(ssr.names_scoring, dplyr::select(dat, !!ssr.names), totals = FALSE, min = 1, max = 5)
  data<-cbind(data, ssr.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"a_ssr"

  return(data)
}
