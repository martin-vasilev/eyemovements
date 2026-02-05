
#' Example raw eye position data  (Eyelink 1000)
#'
#'
#' This data contains one trial of a participant
#' reading a passage from the "Little Wizard Stories of Oz".
#' The data is taken from Slattery and Vasilev (2019) and was
#' recorded with an Eyelink 1000 (at 1000 Hz).
#'
#'
#'
#' @format A data frame with 32,586 rows and 4 variables:
#' \describe{
#'   \item{time}{Time at which eye sample was taken (internal to Eyelink tracker)}
#'   \item{x}{x position of the eye recorded by the Eye-tracker}
#'   \item{y}{y position of the eye recorded by the Eye-tracker}
#'   \item{pupil}{Pupil diameter in pixels}
#' }
#'
#' @source Slattery, T. J., & Vasilev, M. R. (2019). An eye-movement exploration into return-sweep targeting during reading. Attention, Perception, & Psychophysics, 81(5), 1197-1203.
"data_Oz"
