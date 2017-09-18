#' Calculate simulated silhouette to area ratios
#'
#' Recalculate the percentage of potential exposure of flat, tilted surfaces to direct solar radiation with different custom settings of location and time.
#' @param x A dataframe with observations in the rows and at least two spatial position angles in the columns (see Details). Data can come either from 'Ahmes' 1.0 or from measurements performed with traditional instrumentation.
#' @param lat A vector with the latitude of each observation in decimal format. If all observations correspond to the same latitude, \code{lat} can be introduced directly as a single number (see examples).
#' @param long A vector with the longitude of each observation in decimal format. If all observations correspond to the same longitude, \code{long} can be introduced directly as a single number (see examples).
#' @param local.time A numeric vector with the local time of each observation in decimal format. Hours from 00 to 24. Minutes in decimal format, from 0 to 99. E.g., 12:30 should be written as 12.50. If available, seconds should also be specified in decimal format. See \code{\link{fixhour}}. If all observations correspond to the same local time, \code{local.time} can be introduced directly as a single number (see examples).
#' @param tz A vector with the time zone of each observation. If all observations correspond to the same time zone, \code{tz} can be introduced directly as a single number. Time zones located at the West of Greenwich are negative, and at the East are positive; e.g., for Colombia, \code{tz} = -5; for Reunion Island, \code{tz} = 4.
#' @param Ahmes Logical. Do data come from 'Ahmes' 1.0? Defaults to \code{FALSE}.
#' @param ID An optional vector with the labels of the observations. Defaults to \code{NULL}.
#' @param pitch A vector with pitch angles in degrees. See details.
#' @param roll A vector with roll rotation angles in degrees. See details.
#' @param tilt.ang A vector with tilt angles in degrees. Tilt is calculated from pitch and roll angles. This argument allows to specify tilt directly if available.
#' @param horiz Logical. Set the position of the start (zero, 0) of pitch, roll and tilt angle data. \code{horiz = F} indicates the zero is located at zenith. This is the reference system used by Ahmes. \code{horiz = T} indicates the start is at the horizon. To use tilt in further calculations (\code{\link{star}}, \code{\link{sal}}...), angle data should be expressed in the horizontal reference system (0 = horizon). Defaults to \code{TRUE}.
#' @param course A vector with course angles in degrees. See details.
#' @param date A vector containing the date(s) when observations were made. If input data do not come from 'Ahmes', \code{o.format} needs to be specified. See details.
#' @param o.format A character indicating the date format in the input data. It is similar to the argument \code{format} in the function \code{as.Date{base}}. Defaults to \code{NULL}. See details and examples.
#' @param c.hour A vector of custom hours of length equal to or greater than 1. Hours in decimal format, from 0 to 24. Minutes in decimal format, from 0 to 99. E.g., 12:30 should be written as 12.50. If available, seconds should also be specified in decimal format. See \code{\link{fixhour}}. If all observations correspond to the same local time, \code{local.time} can be introduced directly as a single number (see examples).
#' @param c.date A vector of custom dates of length equal to or greater than 1.
#' @param c.long A vector of custom longitude coordinates in decimal format.
#' @param c.lat A vector of custom latitude coordinates in decimal format.
#' @param c.tz A vector of custom time zone/s. Time zones located at the West of Greenwich are negative; e.g., for Colombia, \code{c.tz} = -5; for Reunion Island, \code{c.tz} = 4. 
#' @param LA Indicates whether leaf area data needs to be replicated to match the dimensions of the dataframe corresponding to the custom factors (see 'custom settings' in Details). When \code{LA} is specified, all the five custom factors (\code{c.hour}, \code{c.date}, \code{c.lat}, \code{c.long}, \code{c.tz}) need to be specified (i.e., they should be different from \code{NULL}). Defaults to \code{NULL}. \code{LA} does not affect the calculation of STAR. See examples. 
#' @param details Logical. If (\code{details =FALSE}) the output of \code{simu.star()} is a vector with STAR values. If \code{details = TRUE} the output is the complete dataset plus a column with STAR values. Defaults to \code{TRUE}.
#' @details \code{x} may also content geographical coordinates and hour and date information.
#' @details \code{date} Internally the function uses Julian dates. Julian date is the recommended input format. Conversion tables are available at \url{https://landweb.nascom.nasa.gov/browse/calendar.html} for leap and regular years.
#' @details \code{o.format} Needs to be specified if \code{Ahmes = FALSE} AND input data format is other than Julian or the default formats handled by \code{as.Date{base}} ("\%Y/\%m/\%d" and "\%Y-\%m-\%d"). When \code{Ahmes = TRUE}, \code{o.format} is not needed because functions from the \code{\link{Ahmes}} family solve date issues internally.
#' @details \code{pitch} values span from 0 to 180 degrees. If \code{horiz = TRUE} (default) 0 and 180 refer to the flat horizontal surface and 90 refers to the flat vertical surface. If \code{horiz = FALSE} 0 and 180 refer to the flat vertical surface and 90 refers to the flat horizontal surface.
#' @details \code{roll} values span from 0 to 180 degrees.
#' @details \code{course} values span from 0 (North) to 360 degrees, clockwise. Course is the angle between north and the horizontal projection of a normal vector to the surface.
#' @details For a graphical explanation, see Fig. 2 in Escribano-Rocafort et al. (2014).
#' @details Custom settings. In the functions of the \code{simu} family, the original dataframe is repeated as a block times the number of levels of the custom arguments specified (\code{c.hour}, \code{c.date}, \code{c.lat}, \code{c.long}, and \code{c.tz}). E.g., if a dataset with 10 observations is to be recalculated at two days of the year, the function will create a new dataframe with 20 rows (10 observation X 2 days). For this reason, \code{LA} needs to be specified if the user intends to calculate SAL using the output of \code{simu.star()}. In addition, the argument \code{details} allows the user to visualize the new dataframe created by the function.  
#' @family <star> <simu>
#' @seealso \code{\link{simu.star.app}}, \code{\link{star}}
#' @author Agustina Ventre-Lespiaucq and Silvia Santamaria Bueno.
#' @references \strong{Escribano-Rocafort, A.G., Ventre-Lespiaucq, A.B., Granado-Yela, C., Lopez-Pintor, A., Delgado, J.A., Munoz, V., Dorado, G.A., Balaguer, L. (2014).} Simplifying data acquisition in plant canopies- Measurements of leaf angles with a cell phone. Methods in Ecology and Evolution 5:132-140. doi:10.1111/2041-210X.12141.
#' @keywords  re-calculate star
#' @export
#' @examples 
#' \donttest{# With custom vectors from outside of the dataframe
#'  data(guava)
#'   myhours<-seq(8,16,0.25) # Create a vector of hours from 8:00 to 16:00 every 15 minutes.
#'   mydates<-seq(1,365,60) # Create a vector of days from January 1st to December 31st, 
#'                          # every 60 days.
#'   myguava<-simu.star(guava,lat=40.8,long=-4.2,local.time=6,tz=1,
#'            Ahmes=FALSE,ID=NULL,pitch=guava$pitch,roll=guava$roll,
#'            horiz=FALSE,course=guava$course,date=30,o.format=NULL,
#'            c.hour=myhours,c.date=mydates,c.long=NULL,c.lat=NULL, 
#'            c.tz=NULL, LA=NULL)
#'
#'  # LA is not NULL
#'   wrong<-simu.star(guava,lat=40.8,long=-4.2,local.time=6,tz=1,
#'          Ahmes=FALSE,ID=NULL,pitch=guava$pitch,roll=guava$roll,
#'          horiz=FALSE,course=guava$course,date=30,o.format=NULL,
#'          c.hour=c(7,9.5,12),c.date=c(0,180),LA=guava$LA.cm2,details=TRUE)
#'      # Some custom settings are missing. STAR is retrieved as if LA = NULL, 
#'      # meaning LA data will not be readily available for calculating SAL. 
#'
#'   correct<-simu.star(guava,lat=40.8,long=-4.2,local.time=6,tz=1,
#'            Ahmes=FALSE,ID=NULL,pitch=guava$pitch,roll=guava$roll,
#'            horiz=FALSE,course=guava$course,date=30,o.format=NULL,
#'            c.hour=c(7,9.5,12),c.date=c(0,180),c.lat=40.8,c.long=-4.2, 
#'            c.tz=1, LA=guava$LA.cm2,details=TRUE)
#'      # Returns LA and STAR
#' 
#'  # LA is NULL
#'   correct1<-simu.star(guava,lat=40.8,long=-4.2,local.time=6,tz=1,
#'             Ahmes=FALSE,ID=NULL,pitch=guava$pitch,roll=guava$roll,
#'             horiz=FALSE,course=guava$course,date=30,o.format=NULL,
#'             c.hour=c(7,9.5,12),c.date=c(0,180),c.long=NULL,c.lat=NULL, 
#'             c.tz=NULL, LA=NULL,details=TRUE)
#'           # Is the same as
#'   correct2<-simu.star(guava,lat=40.8,long=-4.2,local.time=6,tz=1,
#'             Ahmes=FALSE,ID=NULL,pitch=guava$pitch,roll=guava$roll,
#'             horiz=FALSE,course=guava$course,date=30,o.format=NULL,
#'             c.hour=c(7,9.5,12),c.date=c(0,180),details=TRUE)}
#' #simu.star() 

simu.star<-function(x,lat=NULL,long=NULL,local.time=NULL,tz=NULL,Ahmes=F,ID=NULL,pitch,roll,tilt.ang=NULL,horiz=T,course,date=NULL,o.format=NULL,c.hour=NULL,c.date=NULL,c.long=NULL,c.lat=NULL,c.tz=NULL, LA=NULL, details=TRUE){
  #IF AHMES IS TRUE:
  if (Ahmes==TRUE){         
    x<-fixfile(x)
    
    latitude=rep(NA,length(x$ID))
    #latitude[1:length(latitude)]=lat
    lat=latitude
    
    longitude=rep(NA,length(x$ID))
    #longitude[1:length(longitude)]=long
    long=longitude
    
    timezone=rep(NA,length(x$ID))
    #timezone[1:length(timezone)]=tz
    tz=timezone
    
    x<-cbind(x,lat,long,tz)
    
    ID=x$ID          
    pitch=x$pitch    
    roll=x$roll      
    course=x$course  
    local.time=x$hour
    date=x$date      
    o.format=NULL
    horiz=F
    
  #IF AHMES IS FALSE:
  } else {
    x2<-as.data.frame(matrix(NA,ncol=9,nrow=nrow(x)))
    colnames(x2)=c("ID","pitch","roll","course","date","hour","lat","long","tz")# rename columns
    x2$ID=ID
    x2$pitch=pitch
    x2$roll=roll
    x2$course=course
    date=c.date
   # x2$hour=local.time
   # x2$lat=lat
   # x2$long=long
   # x2$tz=tz
    x=x2
  }
  if (Ahmes==TRUE){
    tilt.dat<-tilt(x,Ahmes=F,ID=NULL,pitch,roll,horiz=F) 
  } else if ( !is.null(tilt.ang)){
    result<-matrix(NA,nrow=nrow(x),ncol=2) 
    colnames(result)=c("tilt.raw","tilt")
    result[,1]=tilt.ang
    result[,2]=tilt.ang
    tilt.dat<-result
  } else if (horiz==TRUE){
    tilt.dat<-tilt(x,Ahmes=F,ID=NULL,pitch,roll,horiz=T)
  } else {
    tilt.dat<-tilt(x,Ahmes=F,ID=NULL,pitch,roll,horiz=F) 
  }
  tilt.ang=tilt.dat[,2]
  x=cbind(x,tilt.ang)
  #IF AHMES IS TRUE:
  if (Ahmes==TRUE){
    preday <-as.Date(date,format="%d/%m/%Y")
    day<-format(preday, "%j")

    
    #IF AHMES IS FALSE:
  } else if (is.numeric(date)){
    day=date
    
  }  else if (!is.null(o.format)){
    preday <-as.Date(date,format=o.format)
    day<-format(preday, "%j")
    
  } else { 
    preday <-as.Date(date)
    day<-format(preday, "%j")}
  
  day=as.numeric(day)
  
  
  if ( !is.null(c.hour)){
    sim1<-as.data.frame(matrix(NA,ncol=ncol(x),nrow=nrow(x)*length(c.hour)))
    colnames(sim1)=colnames(x)
    sim1[,]=x
    sim1$hour=rep(c.hour,each=nrow(x))
    
  }else {
    sim1=x}
  
  if ( !is.null(c.date)){
    sim2<-as.data.frame(matrix(NA,ncol=ncol(sim1),nrow=nrow(sim1)*length(c.date)))
    colnames(sim2)=colnames(sim1)
    sim2[,]=sim1
    sim2$date=rep(c.date,each=nrow(sim1))
    
  }else {
    sim2=sim1}
  
  if ( !is.null(c.lat)){
    sim3<-as.data.frame(matrix(NA,ncol=ncol(sim2),nrow=nrow(sim2)*length(c.lat)))
    colnames(sim3)=colnames(sim2)
    sim3[,]=sim2
    sim3$lat=rep(c.lat,each=nrow(sim2))  
    
  }else {
    sim3=sim2} 
  
  if ( !is.null(c.long)){
    sim4<-as.data.frame(matrix(NA,ncol=ncol(sim3),nrow=nrow(sim3)*length(c.long)))
    colnames(sim4)=colnames(sim3)
    sim4[,]=sim3
    sim4$long=rep(c.long,each=nrow(sim3))  
    
  }else {
    sim4=sim3}
  
  if ( !is.null(c.tz)){
    sim5<-as.data.frame(matrix(NA,ncol=ncol(sim4),nrow=nrow(sim4)*length(c.tz)))
    colnames(sim5)=colnames(sim4)
    sim5[,]=sim4
    sim5$tz=rep(c.tz,each=nrow(sim4))  
    
  }else {
    sim5=sim4}
 
  if (!is.null(LA)){
    la=LA
    sim6<-as.data.frame(matrix(NA,ncol=ncol(sim5),nrow=nrow(sim5)))
    colnames(sim6)=colnames(sim5)
    sim6[,]=sim5
    hr<-as.factor(c.hour)
    dt<-as.factor(c.date)
    lt<-as.factor(c.lat)
    lng<-as.factor(c.long)
    tzf<-as.factor(c.tz)
    repetitions<-nlevels(hr)*nlevels(dt)*nlevels(lt)*nlevels(lng)*nlevels(tzf)
    if (repetitions==0){ 
      sim6=sim5
      print("Warning: LA cannot not be retrieved because at least one custom parameter is NULL. Specify a vector or a numeric value for c.hour, c.date, c.lat, c.long and c.tz") ### Agus, "and c.tz?" or "or c.tz?"
    } else {
      sim6$LA=rep(la,times=repetitions)  }
  } else {
    sim6=sim5}
 
  STAR<-star(sim6,lat=sim6$lat,long=sim6$long,local.time=sim6$hour,tz=sim6$tz,Ahmes=F,ID=NULL,pitch=sim6$pitch,roll=sim6$roll,horiz=TRUE,tilt.ang=sim6$tilt.ang,course=sim6$course,date=sim6$date,o.format=o.format)# o.format tiene que ser =o.format
  if (details==TRUE){
    return(cbind(sim6,STAR))}
  else {
    return(STAR)}
}

