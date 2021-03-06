#' Leaf angles of guava trees
#'
#' The guava data frame has 239 rows and 11 columns
#' @format This data frame contains the following columns:
#'
#' 		\strong{tree.ID} Identification of the tree. 
#'
#' 		\strong{leaf.ID} Identification of the leaf.
#'
#' 		\strong{measure.ID} Number of observation (from 'Ahmes').
#'
#' 		\strong{sector} Sector of the tree where leaf data was collected, with five levels corresponding to the north, east, south, and west sectors of the crown base (1N, 2E, 3S, 4W, respectively) and the top crown sector (5A).
#'
#' 		\strong{azimuth} Azimuth angle in degrees.
#'
#'		\strong{pitch} Pitch angle in degrees.
#'
#' 		\strong{roll} Roll angle in degrees.
#'
#' 		\strong{course} Course angle in degrees with the zero placed at South (old 'Ahmes' version)
#'
#' 		\strong{LA.cm2} Leaf area. 
#'
#' 		\strong{tilt.degrees} Tilt angle calculated from pitch and roll with a changed reference system (\code{horiz = T}).
#'
#' 		\strong{course.degrees} Course angle in degrees with the zero placed at North (latest 'Ahmes' version 1.0).
#' @details These data are measurements of four leaf position angles of 239 leaves in five guava trees (\emph{Psidium guajava}), each leaf in a row. \code{guava} is an object of class \code{data.frame} and results from applying the function \code{\link{fixfile}} to the output of an earlier version of 'Ahmes', where the reference for course was placed at South. 
#' @references \code{guava} \strong{Ventre-Lespiaucq, A., et al.} Unpublished data.
#' @references \strong{Ahmes 1.0} \url{https://play.google.com/store/apps/details?id=com.movil.hoja.movihoja.ahmes&hl=en}
#' @references \strong{Escribano-Rocafort, A.G., Ventre-Lespiaucq, A.B., Granado-Yela, C., Lopez-Pintor, A., Delgado, J.A., Munoz, V., Dorado, G.A., Balaguer, L. (2014)} Simplifying data acquisition in plant canopies- Measurements of leaf angles with a cell phone. Methods in Ecology and Evolution 5:132-140. doi:10.1111/2041-210X.12141.
"guava"

