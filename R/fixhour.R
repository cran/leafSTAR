#' Fix the hour format
#'
#' Convert the hour from sexagesimal character to decimal numeric format.
#' @param x A character vector with the hour expressed as sexagesimal characters HH:MM or HH:MM:SS.
#' @param seconds Logical. Is the hour expressed as HH:MM:SS (\code{seconds=TRUE}) or as HH:MM (\code{seconds=FALSE})? Defaults to \code{FALSE}.
#' @family <fixers>
#' @author Silvia Santamaria Bueno.
#' @seealso \code{\link{fixfile}}
#' @keywords fixers
#' @export
#' @examples datahours<-c("00:30","10:00","18:20","20:55")
#' @examples  datahours_fixed<-fixhour(datahours,seconds=FALSE)
#' #fixhour()

fixhour<-function(x,seconds=FALSE){

dat=as.character(x)
dat2<-strsplit(dat,":")

if (seconds==FALSE) {
M<-matrix(NA,nrow=length(dat),ncol=2)
for (i in c(1:length(dat))){
  M[i,]=dat2[[i]]}
M2<-matrix(NA,nrow=length(dat),ncol=2)
M2[,]=as.numeric(M)
hour=M2[,1]+(M2[,2]/60)
} else {
M<-matrix(NA,nrow=length(dat),ncol=3)
for (i in c(1:length(dat))){
  M[i,]=dat2[[i]]}
M2<-matrix(NA,nrow=length(dat),ncol=3)
M2[,]=as.numeric(M)
hour=M2[,1]+(M2[,2]/60)+(M2[,3]/3600)
}

return(hour)}

