#' Calculate the silhouette to area ratio from 'Ahmes' 1.0
#'
#' This is a version of \code{star()} function that only works with the output of 'Ahmes' 1.0. This function calculates the percentage of potential exposure of flat, tilted surfaces to direct solar radiation. It is equivalent to the ratio of the surface projected area to total surface area, but instead of using area data it uses spatial position angles (pitch, roll and course or tilt and course), geographical coordinates, hour and date information. This function implements equation 3 in Escribano-Rocafort et al. (2014).
#' @param x Output of 'Ahmes' 1.0, a .csv file.
#' @param lat A vector with the latitude of each observation in decimal format. If all observations correspond to the same latitude, \code{lat} can be introduced directly as a single number (see examples).
#' @param long A vector with the longitude of each observation in decimal format. If all observations correspond to the same longitude, \code{long} can be introduced directly as a single number (see examples).
#' @param tz A vector with the time zone of each observation. If all observations correspond to the same time zone, \code{tz} can be introduced directly as a single number. Time zones located at the West of Greenwich are negative, and at the East are positive; e.g., for Colombia, \code{tz} = -5; for Reunion Island, \code{tz} = 4.
#' @details This function calls \code{\link{fixfile}} internally before calculating STAR. However, it does not modify the original dataset. To have a permanent fixed version of the data, use \code{new.file<-fixfile(your.data)}. If using \code{new.file} within the package, be aware that the argument \code{Ahmes} should be set to \code{FALSE}.
#' @family <star> <'Ahmes'>
#' @seealso \code{\link{fixfile}}, \code{\link{star}}, \code{\link{simu.star.app}}
#' @author Agustina Ventre-Lespiaucq and Silvia Santamaria Bueno.
#' @references \strong{Escribano-Rocafort, A.G., Ventre-Lespiaucq, A.B., Granado-Yela, C., Lopez-Pintor, A., Delgado, J.A., Munoz, V., Dorado, G.A., Balaguer, L. (2014).} Simplifying data acquisition in plant canopies- Measurements of leaf angles with a cell phone. Methods in Ecology and Evolution 5:132-140. doi:10.1111/2041-210X.12141.
#' @keywords  'Ahmes' star
#' @export
#' @examples 
#' \donttest{data(olea)
#'   starapp_olea<-star.app(olea,lat=40,long=4,tz=2)
#'
#'  ## Add results to the original dataset
#'   olea1<-fixfile(olea) ## Fix the original dataset
#'   olea2<-cbind(olea1,as.data.frame(starapp_olea))}
#'
#' #star.app()

star.app<-function(x,lat,long,tz){
 
  x<-fixfile(x)
  ID=x$ID         
  pitch=x$pitch   
  roll=x$roll   
  course=x$course  
  local.time=x$hour
  date=x$date      
  preday <-as.Date(date,format="%d/%m/%Y")
  day<-format(preday, "%j")
  tilt.dat<-tilt(x,Ahmes=F,ID=NULL,pitch,roll,horiz=F) 
   
   
  day=as.numeric(day)

  tilt.dat<-tilt(x,Ahmes=F,ID=NULL,pitch,roll,horiz=F) 
  
  
  rad<-function(deg) {(deg*pi)/180} #convert degrees to radians
  
  X<-rad(360*(day-1)/365.242) 
  
  eot<-(0.258*cos(X)-7.416*sin(X)-3.648*cos(2*X)-9.228*sin(2*X))/60 # in radians
  
  LC.hour<-long/15 + tz 
  solar.time<-local.time+eot-LC.hour # rad
  
  hour.ang<-(solar.time-12)*15 # rad
  
  FYrad<-2*pi*(day-1)/365 
 
  
  decl<-(0.006918-0.399912*cos(FYrad)+0.070257*sin(FYrad)-0.006758*cos(2*FYrad)+0.000907*sin(2*FYrad)-0.002697*cos(3*FYrad)+0.00148*sin(3*FYrad))*180/3.14159 # radians
  
  sun.elev<-sin(rad(decl))*sin(rad(lat))+cos(rad(decl))*cos(rad(lat))*cos(rad(hour.ang))# rad
  
  STAR<-(sun.elev*cos(rad(tilt.dat[,2]))-sin(rad(decl))*cos(rad(lat))*sin(rad(tilt.dat[,2]))*cos(rad(course))+cos(rad(decl))*sin(rad(lat))*cos(rad(hour.ang))*sin(rad(tilt.dat[,2]))*cos(rad(course))+cos(rad(decl))*sin(rad(hour.ang))*sin(rad(tilt.dat[,2]))*sin(rad(course)))*100
  
  return(STAR)
}
