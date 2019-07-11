#' A function that scores the Revised Mosher's Guilt Inventory
#'
#' Calculate's sex-guilt, hostility-guilt and guity-conscience subscale scores.
#' @param dat data frame
#' @param firstq input character for the name of the column for the first
#'variable (e.g., "RMGI_1")
#' @return data frame with new vectors of  scores
#' @export
#' @examples
#' dat = scoreMosherGuilt(dat, firstq = "RMGI_1")
scoreMosherGuilt <- function(dat, firstq){

  itemnum<-114

  firstcol<-match(firstq, names(dat))
  lastcol<-firstcol+itemnum-1
  varnames<-names(dat[firstcol:lastcol])

  sexguilt.names<-varnames[c(6,7,12,13,16,18,25,31,
                             36,42,51,54,61,64,67,71,75,81,83,88,93,102,
                             103,108,112,
                             5,8,11,14,15,17,26,32,35,41,52,
                             53,62,63,78,72,76,82,84,87,94,101,104,107,111)]

  sexguilt.names_scoring<-sexguilt.names

  for(i in 26:length(sexguilt.names_scoring)){
    sexguilt.names_scoring[[i]]<-paste("-", sexguilt.names_scoring[[i]], sep = "")
  }

  sexguilt.scores<-psych::scoreItems(sexguilt.names_scoring, dplyr::select(dat, !!sexguilt.names), totals = TRUE, min = 0, max = 6)
  data<-cbind(dat, sexguilt.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"sex.guilt"

  hostilityguilt.names <-varnames[c(4,19,20,23,30,33,
                                   38,39,43,44,45,55,70,77,79,85,91,95,98,100,
                                   109,113,
                                   3,21,22,24,29,34,37,40,46,56,69,
                                   78,80,86,92,96,97,99,110,114)]

  hostilityguilt.names_scoring <-hostilityguilt.names

  for(i in 23:length(hostilityguilt.names_scoring)){
    hostilityguilt.names_scoring[[i]]<-paste("-", hostilityguilt.names_scoring[[i]], sep = "")
  }

  hostileguilt.scores<-psych::scoreItems(hostilityguilt.names_scoring, dplyr::select(dat, !!hostilityguilt.names), totals = TRUE, min = 0, max = 6)
  data<-cbind(data, hostileguilt.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"hostility.guilt"

  guiltcon.names <-varnames[c(2,10,28,48,49,
                              57,59,65,73,89,105,
                              1,9,27,47,50,58,60,66,74,90,106)]

  guiltcon.names_scoring <-guiltcon.names

  for(i in 12:length(guiltcon.names_scoring)){
    guiltcon.names_scoring[[i]]<-paste("-", guiltcon.names_scoring[[i]], sep = "")
  }

  guiltcon.scores<-psych::scoreItems(guiltcon.names_scoring, dplyr::select(dat, !!guiltcon.names), totals = TRUE, min = 0, max = 6)
  data<-cbind(data, guiltcon.scores$scores)
  names(data)[names(data)==names(data[ncol(data)])]<-"guilty.conscience"

  return(data)
}
