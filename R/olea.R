#' Leaf angles of olive tree measured with 'Ahmes' 1.0
#'
#' The olea data frame has 24 rows and 15 columns
#' @format This data frame contains the following columns:
#'
#' 		\strong{Num.Medida} Number of observation. 
#'
#' 		\strong{Cabeceo} Pitch angle in degrees.
#'
#' 		\strong{Alabeo} Roll angle in degrees.
#'
#' 		\strong{Azimuth} Azimuth angle in degrees.
#'
#' 		\strong{Rumbo} Course angle in degrees.
#'
#' 		\strong{Estado_Brujula} Compass_State. The values depend on the sensors of each cell phone.
#'
#' 		\strong{Estado_Acelerometro} Accelerometer_State. The values depend on the sensors of each cell phone.
#'
#' 		\strong{Estado_Campo_Magnetico} Magnetic_Field_State. The values depend on the sensors of each cell phone.
#'
#' 		\strong{Fecha} Date. Note this is a date-hour column. See details.
#'
#' 		\strong{Hora} Hour. Instead of the hour, it contains the values of the next column. See details.
#'
#' 		\strong{Bateria} Battery. State of the battery.
#'
#' 		\strong{Temperatura} Temperature. Only available when using temperature sensors coupled to the cell phone.  
#'
#' 		\strong{Presion} Pressure. Only available when using pressure sensors coupled to the cell phone.
#'
#' 		\strong{Error_Relativo} Relative error. See Escribano-Rocafort et al. (2014). 
#'
#' 		\strong{Empty column}
#' @details These data are measurements of four leaf position angles of 20 leaves in an olive tree (\emph{Olea europaea}), each leaf in a row. The format of the data is an example of the raw output from 'Ahmes' 1.0. \code{olea} is a .csv file. This file presents some issues that need to be corrected. There are four extra rows to be deleted (1:3, final row). The hour and date are in the same colum, which needs to be separated in order to extract hour and date information directly from the data. This makes column headers to be displaced from columns 10 to 15. In addition, the headers of the output of Ahmes are provided in Spanish. Function \code{\link{fixfile}} implements solutions to these issues and renames the variables in English.
#' @references \code{olea} \strong{Ventre-Lespiaucq, A., Santamaria Bueno, S.}, unpublished data.
#' @references \strong{Ahmes 1.0} \url{https://play.google.com/store/apps/details?id=com.movil.hoja.movihoja.ahmes&hl=en}
#' @references \strong{Escribano-Rocafort, A.G., Ventre-Lespiaucq, A.B., Granado-Yela, C., Lopez-Pintor, A., Delgado, J.A., Munoz, V., Dorado, G.A., Balaguer, L. (2014)} Simplifying data acquisition in plant canopies- Measurements of leaf angles with a cell phone. Methods in Ecology and Evolution 5:132-140. doi:10.1111/2041-210X.12141.
"olea"

