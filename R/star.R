#' Calculate the silhouette to area ratio
#'
#' Calculate the percentage of potential exposure of flat, tilted surfaces to direct solar radiation. It is equivalent to the ratio of the surface projected area to total surface area, but instead of using area data it uses spatial position angles (pitch, roll and course or tilt and course), geographical coordinates, hour and date information. This function implements equation 3 in Escribano-Rocafort et al. (2014).
#' @param x A dataframe with observations in the rows and at least two spatial position angles in the columns (see Details). Data can either come from 'Ahmes' 1.0 or from measurements performed with traditional instrumentation.
#' @param lat A vector with the latitude of each observation in decimal format. If all observations correspond to the same latitude, \code{lat} can be introduced directly as a single number (see examples).
#' @param long A vector with the longitude of each observation in decimal format. If all observations correspond to the same longitude, \code{long} can be introduced directly as a single number (see examples).
#' @param local.time A numeric vector with the approximate local time of each observation in decimal format. Hours from 0 to 24. Minutes in decimal format, from 0 to 99. E.g., 12:30 should be written as 12.50. If available, seconds should also be specified in decimal format. See \code{\link{fixhour}}. If all observations correspond to the same local time, \code{local.time} can be introduced directly as a single number (see examples).
#' @param tz A vector with the time zone of each observation. If all observations correspond to the same time zone, \code{tz} can be introduced directly as a single number (see examples). Time zones located at the West of Greenwich are negative, and at the East are positive; e.g., for Colombia, \code{tz} = -5; for Reunion Island, \code{tz} = 4.
#' @param Ahmes Logical. Do data come from 'Ahmes' 1.0? Defaults to \code{FALSE}.
#' @param ID An optional vector with the labels of the observations. Defaults to \code{NULL}.
#' @param pitch A vector with pitch angles in degrees. See details.
#' @param roll A vector with roll rotation angles in degrees. See details.
#' @param tilt.ang A vector with tilt angles in degrees. Tilt is calculated from \code{pitch} and \code{roll} angles. This argument allows to specify tilt directly if available.
#' @param horiz Logical. Set the position of the start (zero, 0) of pitch, roll and tilt angle data. \code{horiz = F} indicates the zero is located at zenith. This is the reference system used by 'Ahmes'. \code{horiz = T} indicates the start is at the horizon. To use tilt in further calculations (\code{\link{simu.star}}, \code{\link{sal}}...), angle data should be expressed in the horizontal reference system (0 = horizon). Defaults to \code{TRUE}.
#' @param course A vector with course angles in degrees. See details.
#' @param date A vector containing the date(s) when observations were made. If input data do not come from 'Ahmes', \code{o.format} needs to be specified. See details.
#' @param o.format A character indicating the date format in the input data. It is similar to the argument \code{format} in the function \code{as.Date{base}}. Defaults to \code{NULL}. See details and examples.
#' @details \code{x} may also content geographical coordinates and hour and date information.
#' @details \code{date} Internally the function uses Julian dates. Julian date is the recommended input format. Conversion tables are available at \url{https://landweb.nascom.nasa.gov/browse/calendar.html} for leap and regular years.
#' @details \code{o.format} Needs to be specified if \code{Ahmes = FALSE} AND input data format is other than Julian or the default formats handled by \code{as.Date{base}} ("\%Y/\%m/\%d" and "\%Y-\%m-\%d"). When \code{Ahmes = TRUE}, \code{o.format} is not needed because functions of the \code{\link{Ahmes}} family solve date issues internally.
#' @details \code{pitch} values span from 0 to 180 degrees. If \code{horiz = TRUE} (default) 0 and 180 refer to the flat horizontal surface and 90 refers to the flat vertical surface. If \code{horiz = FALSE} 0 and 180 refer to the flat vertical surface and 90 refers to the flat horizontal surface. 
#' @details \code{roll} values span from 0 to 180 degrees. 
#' @details \code{course} values span from 0 (North) to 360 degrees, clockwise. Course is the angle between north and the horizontal projection of a normal vector to the surface.
#' @details For a graphical explanation, see Fig. 2 in Escribano-Rocafort et al. (2014).
#' @family <star>
#' @author Agustina Ventre-Lespiaucq and Silvia Santamaria Bueno.
#' @references \strong{Escribano-Rocafort, A.G., Ventre-Lespiaucq, A.B., Granado-Yela, C., Lopez-Pintor, A., Delgado, J.A., Munoz, V., Dorado, G.A., Balaguer, L. (2014).} Simplifying data acquisition in plant canopies- Measurements of leaf angles with a cell phone. Methods in Ecology and Evolution 5:132-140. doi:10.1111/2041-210X.12141.
#' @keywords star
#' @export
#' @examples 
#' \donttest{## Example with an 'Ahmes'-type input
#' data(olea)
#' star_olea<-star(olea,lat=40,long=4,tz=2,Ahmes=TRUE)
#'  # Add results to the original dataset
#'     olea1<-fixfile(olea) # Fix the original dataset
#'     olea2<-cbind(olea1,as.data.frame(star_olea))
#' 
#'  # Example with an input different to 'Ahmes' data
#'   data(olive)
#'    star_olive<-star(olive,lat=olive$latitude,long=olive$longitude,
#'                local.time=olive$hour,tz=olive$tz, Ahmes=FALSE,
#'                ID=olive$leafID,tilt.ang=olive$tilt,horiz=TRUE,
#'                course=olive$course,date=olive$date)
#'   # Add results to the original dataset.
#'    olive2<-cbind(olive,as.data.frame(star_olive)) # Since it does not
#'                                                  # come from 'Ahmes', 
#'                                                  # it is not necessary 
#'                                                  # to run fixfile().
#' 
#' ## Input date formats. The three examples give the same result.
#'    # 1. With Julian date
#'         julian_olive<-star(olive,lat=olive$latitude,long=olive$longitude,
#'                       local.time=olive$hour,tz=olive$tz,
#'                       Ahmes=FALSE,ID=olive$leafID,tilt.ang=olive$tilt,horiz=TRUE,
#'                       course=olive$course,date=25) # January 25th
#'  
#'    # 2. With standard date
#'         std_olive<-star(olive,lat=olive$latitude,long=olive$longitude,
#'                    local.time=olive$hour,tz=olive$tz,
#'                    Ahmes=FALSE,ID=olive$leafID,tilt.ang=olive$tilt,horiz=TRUE,
#'                    course=olive$course,date="2017/01/25") # January 25th 2017.
#'                          # Date should be quoted. o.format is not needed.
#' 
#'    # 3. With non-standard date
#'         nonstd_olive<-star(olive,lat=olive$latitude,long=olive$longitude,
#'                       local.time=olive$hour,tz=olive$tz, Ahmes=FALSE,
#'                       ID=olive$leafID,tilt.ang=olive$tilt,horiz=TRUE,
#'                       course=olive$course,date="25/01/2017",o.format="%d/%m/%Y") 
#'                # January 25th 2017. Date should be quoted. o.format is necessary.
#' }
#' # star()


star<-function(x,lat,long,local.time=NULL,tz,Ahmes=F,ID=NULL,pitch=NULL,roll=NULL,horiz=T,tilt.ang=NULL,course=NULL,date=NULL,o.format=NULL){
  
  if (Ahmes==TRUE){
    x<-fixfile(x)
    ID=x$ID          
    pitch=x$pitch    
    roll=x$roll 
    local.time=x$hour
    course=x$course  
    date=x$date      
    preday <-as.Date(date,format="%d/%m/%Y")
    day<-format(preday, "%j")
    tilt.dat<-tilt(x,Ahmes=F,ID=NULL,pitch,roll,horiz=F) 
    
  } else if (is.numeric(date)){
    day=date
    
  } else if ( !is.null(o.format)){
    preday <-as.Date(date,format=o.format)
    day<-format(preday, "%j")
    
  } else{ 
    preday <-as.Date(date)
    day<-format(preday, "%j")
    
  }
  day=as.numeric(day)
  
  if ( !is.null(tilt.ang)){
    result<-matrix(NA,nrow=nrow(x),ncol=2) 
    colnames(result)=c("tilt.raw","tilt")
    result[,1]=tilt.ang
    result[,2]=tilt.ang
    tilt.dat<-result
    
  }
  else if (horiz==TRUE){
    tilt.dat<-tilt(x,Ahmes=F,ID=NULL,pitch,roll,horiz=T)
  } else{
    tilt.dat<-tilt(x,Ahmes=F,ID=NULL,pitch,roll,horiz=F) 
  }
  
  rad<-function(deg) {(deg*pi)/180} #convert degrees to radians
  
  X<-rad(360*(day-1)/365.242) # in radians
  
  eot<-(0.258*cos(X)-7.416*sin(X)-3.648*cos(2*X)-9.228*sin(2*X))/60 # in radians
  
  LC.hour<-long/15 + tz # degrees
  
  solar.time<-local.time+eot-LC.hour # rad
  
  hour.ang<-(solar.time-12)*15 # rad
  
  FYrad<-2*pi*(day-1)/365 # in radians, which will give decl in radians
  # day: Number of days since midnight Coordinated Universal Time as January 1st begins
  
  decl<-(0.006918-0.399912*cos(FYrad)+0.070257*sin(FYrad)-0.006758*cos(2*FYrad)+0.000907*sin(2*FYrad)-0.002697*cos(3*FYrad)+0.00148*sin(3*FYrad))*180/3.14159 # radians
  
  sun.elev<-sin(rad(decl))*sin(rad(lat))+cos(rad(decl))*cos(rad(lat))*cos(rad(hour.ang))# rad
  
  STAR<-(sun.elev*cos(rad(tilt.dat[,2]))-sin(rad(decl))*cos(rad(lat))*sin(rad(tilt.dat[,2]))*cos(rad(course))+cos(rad(decl))*sin(rad(lat))*cos(rad(hour.ang))*sin(rad(tilt.dat[,2]))*cos(rad(course))+cos(rad(decl))*sin(rad(hour.ang))*sin(rad(tilt.dat[,2]))*sin(rad(course)))*100
  
  return(STAR)
}
