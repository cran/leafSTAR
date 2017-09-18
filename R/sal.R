#' Calculate the projected surface area of a tilted plane
#'
#' Retrieves the projected area of the surface in area units.
#' @param x Numeric vector, commonly the output of functions of the \code{\link{star}} family, containing STAR (\%).
#' @param LA Numeric vector containing the total area of the surface of each observation.
#' @seealso SAL can be calculated with STAR data coming from \code{\link{star}}, \code{\link{simu.star}}, \code{\link{star.app}}, and \code{\link{simu.star.app}}
#' @author Agustina Ventre-Lespiaucq and Silvia Santamaria Bueno.
#' @export
#' @examples 
#' \donttest{data(guava)
#'  ## Calculate SAL for a given location, date and time
#'     gua<-star(guava,lat=40.82,long=-4.21,local.time=12,tz=1,
#'          Ahmes=FALSE,ID=NULL,pitch=guava$pitch,roll=guava$roll,
#'          horiz=FALSE,course=guava$course,date=30,o.format=NULL)
#'          sal(gua,LA=guava$LA.cm2)
#' 
#'  ## Calculate SAL from simu.star() 
#'     sim.gua<-simu.star(guava,lat=40.82,long=-4.21,local.time=12,tz=1,
#'              Ahmes=FALSE,ID=NULL, pitch=guava$pitch,roll=guava$roll,
#'              horiz=FALSE,course=guava$course,date=30,o.format=NULL,
#'              c.hour=c(7,9.5,12),c.date=30,c.lat=40.8,c.long=-4.2,c.tz=1,
#'              LA=guava$LA.cm2,details=TRUE)
#'              sal.guava<-sal(sim.gua$STAR,LA=sim.gua$LA)}
#' #sal()

sal<-function(x,LA){
  S=x*LA/100
  S1<-as.data.frame(S) 
	names(S1)<-"SAL.area"
  return(S1)
}
