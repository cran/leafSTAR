#' Calculate the dihedral angle between two planes
#'
#' Calculate the minimum angle between two non-orthogonal planes, such as the angle describing leaf torsion, the angle between two halves of conduplicate or plicate leaves, the angle between the petiole and the branch, between two leaves and between two branches.
#' @param Ahmes Logical. Do data come from 'Ahmes'? Defaults to FALSE
#' @param ID An optional vector with the labels of the observations. Defaults to \code{NULL}.
#' @param plane1,plane2 Objects of class data.frame containing the angles of plane1 and plane2 and row labels.
#' @param pitch1,pitch2 Numeric. The name of the variables containing the pitch angles of planes 1 and 2 in degrees. See details.
#' @param roll1,roll2 Numeric. The name of the variables containing the roll angles of planes 1 and 2 in degrees. See details.
#' @param course1,course2 Numeric. The name of the variables containing the course angles of planes 1 and 2 in degrees. See details.
#' @param horiz Logical. Set the position of the start (zero, 0) of pitch, roll and tilt angle data. \code{horiz = F} indicates the zero is located at zenith. This is the reference system used by Ahmes. \code{horiz = T} indicates the start is at the horizon. To use tilt in further calculations (\code{\link{star}}, \code{\link{sal}}...), angle data should be expressed in the horizontal reference system (0 = horizon). Defaults to \code{TRUE}.
#' @details \code{pitch} values span from 0 to 180 degrees. If \code{horiz = TRUE} (default) 0 and 180 refer to the flat horizontal surface and 90 refers to the flat vertical surface. If \code{horiz = FALSE} 0 and 180 refer to the flat vertical surface and 90 refers to the flat horizontal surface.
#' @details \code{roll} values span from 0 to 180 degrees.
#' @details \code{course} values span from 0 (North) to 360 degrees, clockwise. Course is the angle between north and the horizontal projection of a normal vector to the surface.
#' @details For a graphical explanation of leaf angles, see Fig. 2 in  Escribano-Rocafort et al. (2014).
#' @author Agustina Ventre-Lespiaucq and Silvia Santamaria Bueno.
#' @references \code{dihedral} \strong{Santalo, L.A. (1970).} Vectores y tensores con sus aplicaciones. p. 61, 8 Edicion. EUDEBA (eds), Buenos Aires, Argentina. 
#' @references \strong{Escribano-Rocafort, A.G., Ventre-Lespiaucq, A.B., Granado-Yela, C., Lopez-Pintor, A., Delgado, J.A., Munoz, V., Dorado, G.A., Balaguer, L. (2014).} Simplifying data acquisition in plant canopies- Measurements of leaf angles with a cell phone. Methods in Ecology and Evolution 5:132-140. doi:10.1111/2041-210X.12141.
#' @references \code{orchids} \strong{Ventre-Lespiaucq, A.B., Delgado, J.A., Ospina-Calderon, N.H., Otero, J.T., Escudero, A., Sanchez, M.A., Balaguer, L., Flanagan, N.S. (2017).} A tropical epiphytic orchid uses a low-light interception strategy in a spatially heterogeneous light environment. Biotropica, 49:318-327. doi:10.1111/btp.12425.
#' @export
#' @examples 
#' \donttest{data(orchids)
#'   pseudobulbs<-subset(orchids,organ=="pseudobulb") #subset
#'   leaves<-subset(orchids,organ=="leaf")
#'   dihedral(plane1=pseudobulbs,plane2=leaves,Ahmes=FALSE,ID=NULL,pitch1=pseudobulbs$pitch,
#'   roll1=pseudobulbs$roll,course1=pseudobulbs$course,pitch2=leaves$pitch,roll2=leaves$roll,
#'   course2=leaves$course,horiz=FALSE)}  
#' #dihedral()


dihedral<- function(plane1,plane2,Ahmes=FALSE,ID=NULL,pitch1,roll1,course1,pitch2,roll2,course2,horiz=TRUE) {

 if (Ahmes==TRUE){
#actions for plane1
  x1<-fixfile(plane1)
  ID1=x1$ID         
  pitch1=x1$pitch   
  roll1=x1$roll     
  course1=x1$course 

#actions for plane2
  x2<-fixfile(plane2)
  ID2=x2$ID          
  pitch2=x2$pitch   
  roll2=x2$roll      
  course2=x2$course
  
  tilt1<-tilt(x1,Ahmes=FALSE,ID=NULL,pitch1,roll1,horiz=F)  
  tilt2<-tilt(x2,Ahmes=FALSE,ID=NULL,pitch2,roll2,horiz=F) 
  
  } else {

  x1<-plane1
  x2<-plane2

  tilt1<-tilt(x1,Ahmes=FALSE,ID=NULL,pitch1,roll1,horiz=horiz)  
  tilt2<-tilt(x2,Ahmes=FALSE,ID=NULL,pitch2,roll2,horiz=horiz) 
  
   }

	### The equation begins here
	deg2rad<- function(deg) {(deg * pi) / (180)}
	tilt1.rad<-deg2rad(tilt1)
	tilt2.rad<-deg2rad(tilt2)
	course1.rad<-deg2rad(course1)
	course2.rad<-deg2rad(course2)	

	theta<-matrix(NA, nrow=nrow(x1),ncol=2)
	#functions needed
	
for (i in 1:nrow(x1) & 1:nrow(x2)) {
	#components of the normal vectors (x1,x2,x3)

		# of N leaf (plane 1)
		a1=cos(tilt1.rad[,2])
		a2=sin(tilt1.rad[,2])*cos(course1.rad)
		a3=sin(tilt1.rad[,2])*sin(course1.rad)
		
		# of N pseudobulb (plane 2)
		b1=cos(tilt2.rad[,2])
		b2=sin(tilt2.rad[,2])*cos(course2.rad)
		b3=sin(tilt2.rad[,2])*sin(course2.rad)

	# calculate theta as the acos of the scalar product of plane 1 and 2
	theta.rad=acos(a1*b1+a2*b2+a3*b3)

	#convert to degrees
	rad2deg <- function(rad) {(rad * 180) / (pi)}
	theta.deg=rad2deg(theta.rad)
}
	theta[i,1]<-theta.rad
	theta[i,2]<-theta.deg
	colnames(theta)<-c("theta.rad","theta.deg")

return(theta)

}



