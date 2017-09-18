#' Fix the output of 'Ahmes' 1.0
#'
#' Convert the output of 'Ahmes' 1.0 to a dataframe that can be handled by \code{'leafSTAR'} and \code{'R'} in general.
#' @param x The output of 'Ahmes' 1.0. A .csv file
#' @details This function does not modify the original dataset. To have a permanent fixed version of the data, see Example. If using the new data within the package, be aware that the argument \code{Ahmes} should be set to \code{FALSE}.
#' @family <fixers> <'Ahmes'>
#' @seealso \code{\link{Ahmes}}, \code{\link{fixhour}}
#' @author Agustina Ventre-Lespiaucq and Silvia Santamaria Bueno.
#' @keywords 'Ahmes' fixers
#' @export
#' @examples 
#' \donttest{data(olea)
#'  olea_fixed<-fixfile(olea) ## Store the fixed file as a new object.}
#' #fixfile()


fixfile<-function(x){
  
  wh<-x
  wh1<-as.matrix(wh) 
  
  input<-wh1[-c(1:3),] 
  input<-input[-nrow(input),] 
  
  data<-matrix(NA, nrow=nrow(input),ncol=ncol(input)) 
  names=rep(NA,ncol(input))
  names[1:14]=c("ID","pitch","roll","azimuth","course","Compass_status","Accelerometer_status","MagField_status","date","hour","Battery","Temperature","Pressure","Rel_Err")
  colnames(data)<-names
  
  data[,2:5]<-apply(input[,2:5],2,function(x) {as.numeric(gsub(",",".",x,fixed=TRUE))}) 
  
  data[,1]<-as.matrix(as.numeric(input[,1]),byrow=TRUE) 
  data[,c(6:8,11:14)]<-as.matrix(input[,c(6:8,11:14)])
  
  out <- strsplit(as.character(input[,9])," ") 
  data[,9:10]<-do.call(rbind, out) 
  data=as.data.frame(data)
  for (i in c(1:8)){
    data[,i]<-as.numeric(as.character(data[,i]))}
  
  dat=as.character(data$hour)
  dat2<-strsplit(dat,":")
  M<-matrix(NA,nrow=length(dat),ncol=3)
  for (i in c(1:length(dat))){
    M[i,]=dat2[[i]]}
  M2<-matrix(NA,nrow=length(dat),ncol=3)
  M2[,]=as.numeric(M)
  data$hour=M2[,1]+(M2[,2]/60)+(M2[,3]/3600)
  
  return(data)
}
