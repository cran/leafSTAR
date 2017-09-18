#' Leaf angles of olive tree measured with a compass and inclinometer
#'
#' The olive data frame has 20 rows and 10 columns
#' @format This data frame contains the following columns:
#'
#' 		\strong{location} Where measurements were taken. 
#'
#' 		\strong{latitude} Latitude where measurements were taken, in decimal format.
#'
#' 		\strong{longitude} Longitude where measurements were taken, in decimal format.
#'
#' 		\strong{date} Date.
#'
#' 		\strong{hour} Hour.
#'
#' 		\strong{tz} UTC time zone. Time zones west of Greenwich should be indicated with a minus sign (e.g., \code{tz = -2}).
#'
#' 		\strong{leaf.ID} Identification of the leaf.
#'
#' 		\strong{tilt} Tilt angle measured with a digital clinometer.
#'
#' 		\strong{course} Course angle measured with a compass.
#'
#' 		\strong{azimuth} Azimuth angle measured with a compass.
#' @details These data are measurements of three leaf position angles of 20 leaves in an olive tree (\emph{Olea europaea}), each leaf in a row. This dataset is an example of compass and inclinometer measurements. Tilt was measured instead of pitch and roll. The data.frame includes data on the geographic location, date and time. All these variables can be used directly as arguments in the functions of the \code{\link{star}} family.
#' @references \code{olive} \strong{Ventre-Lespiaucq, A., Santamaria Bueno, S.}, unpublished data.
#' @references \strong{Escribano-Rocafort, A.G., Ventre-Lespiaucq, A.B., Granado-Yela, C., Lopez-Pintor, A., Delgado, J.A., Munoz, V., Dorado, G.A., Balaguer, L. (2014)} Simplifying data acquisition in plant canopies- Measurements of leaf angles with a cell phone. Methods in Ecology and Evolution 5:132-140. doi:10.1111/2041-210X.12141.
"olive"

