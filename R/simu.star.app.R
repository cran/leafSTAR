#' Calculate simulated silhouette to area ratios from 'Ahmes' 1.0
#'
#' This is a version of simu.star function that only works with the output of 'Ahmes' 1.0. \code{simu.star.app()} re-calculates the percentage of potential exposure of flat, tilted surfaces to direct solar radiation with different custom settings of location and time.
#' @param x Output of 'Ahmes' 1.0. A .csv file.
#' @param lat A vector with the latitude of each observation in decimal format. If all observations correspond to the same latitude, \code{lat} can be introduced directly as a single number (see examples).
#' @param long A vector with the longitude of each observation in decimal format. If all observations correspond to the same longitude, \code{long} can be introduced directly as a single number (see examples).
#' @param tz A vector with the time zone of each observation. If all observations correspond to the same time zone, \code{tz} can be introduced directly as a single number (see examples). Time zones located at the West of Greenwich are negative, and at the East are positive; e.g., for Colombia, \code{tz} = -5; for Reunion Island, \code{tz} = 4.
#' @param c.hour A vector of custom hours of length equal to or greater than 1. Hours in decimal format, from 0 to 24. Minutes in decimal format, from 0 to 99. E.g., 12:30 should be written as 12.50. If available, seconds should also be specified in decimal format. See \code{\link{fixhour}}. If all observations correspond to the same local time, \code{local.time} can be introduced directly as a single number (see examples).
#' @param c.date A vector of custom dates of length equal to or greater than 1.
#' @param c.long A vector of custom longitude coordinates in decimal format.
#' @param c.lat A vector of custom latitude coordinates in decimal format.
#' @param c.tz A vector of custom time zone/s. Time zones located at the West of Greenwich are negative; e.g., for Colombia, \code{c.tz} = -5; for Reunion Island, \code{c.tz} = 4. 
#' @param LA Indicates whether leaf area data needs to be replicated to match the dimensions of the dataframe corresponding to the custom factors (see 'custom settings' in Details). When \code{LA} is specified, all the five custom factors (\code{c.hour}, \code{c.date}, \code{c.lat}, \code{c.long}, \code{c.tz}) need to be specified (i.e., they should be different from \code{NULL}). Defaults to \code{NULL}. \code{LA} does not affect the calculation of STAR. See examples. 
#' @param details Logical. If (\code{details =FALSE}), the output of \code{simu.star.app()} is a vector with STAR values. If \code{details = TRUE} the output is the complete dataset plus a column with STAR values. Defaults to \code{TRUE}.
#' @param o.format A character indicating the date format in the input data. It is similar to the argument \code{format} in the function \code{as.Date{base}}. Defaults to \code{NULL}. See details and examples.
#' @details This function calls \code{\link{fixfile}} internally before calculating STAR. However, it does not modify the original dataset. To have a permanent fixed version of the data, use \code{new.file<-fixfile(your.data)}. If using \code{new.file} within the package, be aware that the argument \code{Ahmes} should be set to \code{FALSE}.   
#' @details \code{o.format} Needs to be specified if \code{Ahmes = FALSE} AND input data format is other than Julian or the default formats handled by \code{as.Date{base}} ("\%Y/\%m/\%d" and "\%Y-\%m-\%d"). When \code{Ahmes = TRUE}, \code{o.format} is not needed because functions of the \code{\link{Ahmes}} family solve date issues internally.
#' @details Custom settings. In the functions of the \code{simu} family, the original dataframe is repeated as a block times the number of levels of the custom arguments specified (\code{c.hour}, \code{c.date}, \code{c.lat}, \code{c.long}, and \code{c.tz}). E.g., if a dataset with 10 observations is to be recalculated at two days of the year, the function will create a new dataframe with 20 rows (10 observation X 2 days). For this reason, \code{LA} needs to be specified if the user intends to calculate SAL using the output of \code{simu.star}. In addition, the argument \code{details} allows the user to visualize the new dataframe created by the function.  
#' @family <star> <'Ahmes'> <simu>
#' @seealso \code{\link{fixfile}}, \code{\link{simu.star}}, \code{\link{star.app}}
#' @author Agustina Ventre-Lespiaucq and Silvia Santamaria Bueno.
#' @references \strong{Escribano-Rocafort, A.G., Ventre-Lespiaucq, A.B., Granado-Yela, C., Lopez-Pintor, A., Delgado, J.A., Munoz, V., Dorado, G.A., Balaguer, L. (2014).} Simplifying data acquisition in plant canopies- Measurements of leaf angles with a cell phone. Methods in Ecology and Evolution 5:132-140. doi:10.1111/2041-210X.12141.
#' @keywords star 'Ahmes' re-calculate
#' @export
#' @examples data(olea)
#' @examples simustar_olea<-simu.star.app(olea,lat=40,long=4,tz=2,
#' @examples  c.hour=c(8,12,18),c.date=c(1,60,120,180,240))
#'
#' #simu.star.app()
simu.star.app<-function(x,lat,long,tz,o.format=NULL,c.hour=NULL,c.date=NULL,c.long=NULL,c.lat=NULL,c.tz=NULL,LA=NULL,details=FALSE){
         
    x<-fixfile(x)
    
    latitude=c(1:length(x$ID))
    latitude[1:length(latitude)]=lat
    lat=latitude
    
    longitude=c(1:length(x$ID))
    longitude[1:length(longitude)]=long
    long=longitude
    
    timezone=c(1:length(x$ID))
    timezone[1:length(timezone)]=tz
    tz=timezone
    
    x<-cbind(x,lat,long,tz)
    
    ID=x$ID         
    pitch=x$pitch    
    roll=x$roll     
    course=x$course 
    local.time=x$hour
    date=x$date     
    horiz=FALSE
    preday <-as.Date(date,format="%d/%m/%Y")
    day<-format(preday, "%j")
    tilt.dat<-tilt(x,Ahmes=F,ID=NULL,pitch,roll,horiz=F) # 
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
 
    STAR<-star(sim6,lat=sim6$lat,long=sim6$long,local.time=sim6$hour,tz=sim6$tz,Ahmes=F,ID=NULL,pitch=sim6$pitch,roll=sim6$roll,horiz=TRUE,course=sim6$course,date=sim6$date,o.format=o.format)# o.format tiene que ser =o.format
    
    if (details==TRUE){
      return(cbind(sim5,STAR))}
    else {
      return(STAR)} 
   }


