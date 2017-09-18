#' Calculate the tilt angle
#'
#' Calculate the tilt angle of a tilted surface from pitch and roll angles.
#' @param x A dataframe with observations in the rows and at least two spatial position angles in the columns. Data can come either from 'Ahmes' 1.0 or from measurements performed with traditional instrumentation.
#' @param Ahmes Logical. Do data come from 'Ahmes'? Defaults to FALSE.
#' @param ID An optional vector with the labels of the observations. Defaults to \code{NULL}.
#' @param pitch A vector with pitch angles in degrees. See details.
#' @param roll A vector with roll angles in degrees. See details.
#' @param horiz Logical. Set the position of the start (zero, 0) of pitch, roll and tilt angle data. \code{horiz = F} indicates the zero is located at zenith. This is the reference system used by 'Ahmes'. \code{horiz = T} indicates the start is at the horizon. To use tilt in further calculations (\code{\link{star}}, \code{\link{sal}}...), angle data should be expressed in the horizontal reference system (0 = horizon). Defaults to \code{TRUE}.
#' @details The output of \code{tilt()} is a matrix with two columns "tilt.raw" and "tilt". "tilt.raw" contains tilt values relative to the zenith. "tilt" contains tilt values relative to the horizon. The latter are used in the equations of the \code{star()} family. When \code{horiz = T}, "tilt" = "tilt.raw".
#' @details \code{pitch} values span from 0 to 180 degrees. If \code{horiz = TRUE} (default) 0 and 180 refer to the flat horizontal surface and 90 refers to the flat vertical surface. If \code{horiz = FALSE} 180 refers to the flat vertical surface and 90 refers to the flat horizontal surface.
#' @details \code{roll} values span from 0 to 180 degrees.
#' @details For a graphical explanation, see Fig. 2 in Escribano-Rocafort et al. (2014).
#' @family \code{\link{Ahmes}}
#' @keywords 'Ahmes'
#' @author Agustina Ventre-Lespiaucq and Silvia Santamaria Bueno.
#' @references \strong{Escribano-Rocafort, A.G., Ventre-Lespiaucq, A.B., Granado-Yela, C., Lopez-Pintor, A., Delgado, J.A., Munoz, V., Dorado, G.A., Balaguer, L. (2014).} Simplifying data acquisition in plant canopies- Measurements of leaf angles with a cell phone. Methods in Ecology and Evolution 5:132-140. doi:10.1111/2041-210X.12141.
#' @references \code{tropical} \strong{Posada, J.M., Lechowicz, M.J., Kitajima, K. (2009).} Optimal photosynthetic use of light by tropical tree crowns achieved by adjustment of individual leaf angles and nitrogen content. Annals of Botany, 103: 795-805. doi:10.1093/aob/mcn265.
#' 
#' @export
#' @examples 
#' \donttest{# Data comes from 'Ahmes'
#' data(olea)
#'     tilt_olea<-tilt(olea,Ahmes=TRUE)
#'
#'  # Data comes from other sources
#'    data(tropical)
#'     tropi_tilt<-tilt(tropical,pitch=tropical$pitch,roll=tropical$roll,horiz=TRUE)
#'     tropical2<-cbind(tropical,as.data.frame(tropi_tilt)) 
#'     # When horiz = TRUE, tilt.raw = tilt!
#' 
#'    data(guava)
#'     tilt_guava<-tilt(guava,pitch=guava$pitch,roll=guava$roll,horiz=FALSE) 
#'     # horiz = FALSE}
#'
#' #tilt()

tilt<-function(x,Ahmes=F,ID=NULL,pitch,roll,horiz=T){
  if (Ahmes==TRUE){                             # Is the database the output of Ahmes.app?
    x<-fixfile(x)                               # If yes, fix it
    pitch=x$pitch    
    roll=x$roll      
    horiz=F
    
  }else {                                       # If not, do not change it
    x=x}
    pitch=pitch
    roll=roll
    rad<-function(deg) {(deg*pi)/180} 
    result<-matrix(NA,nrow=nrow(x),ncol=2)      # Generate matrix to store results
    colnames(result)=c("tilt.raw","tilt")       # name the columns in the new matrix
    result[,1]<-asin(sin(rad(pitch))*cos(rad(roll)))*180/pi # Column containing raw tilt (with the reference system of the cell phone) calculated in radians and converted to degrees

  if (horiz==F){
  
    for (i in 1:nrow(x)){ # function to obtain tilt in other system of reference (with X= horizontal and 0 = vertical)
      if (result[i,1] >= 0) {
        result[i,2]<-90-abs(result[i,1])
      } else {   result[i,2]<-abs((result[i,1])-90)
        }}
      
      if  (!is.null(ID)){
        ID=ID
      rownames(result)=ID # keep the labels of each row
      }} else{
result[,2]=result[,1]}
    
return(result) 
}

