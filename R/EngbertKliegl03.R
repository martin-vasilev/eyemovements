#' @importFrom magrittr %>%
NULL


#' Detect fixations or saccades using the Engbert and Kliegl (2003) algorithm
#'
#' Detects fixations or saccades from eye-tracking samples using the velocity-based
#' algorithm proposed by Engbert and Kliegl (2003). Velocities are
#' estimated using a 5-sample moving window and saccades are identified
#' using robust median-based elliptical velocity thresholds. The \code{lambda}
#' parameter determines the sensitivity of the algorithm. Note that
#' Engbert and Kliegl (2003) use a default lambda of 6 for microsaccade research.
#' However, users can increase that for other applications (espcially if a lot of short fixations are not desirable).

#' By default, the function returns fixation data.
#'To return saccade data, set `return_saccades= T`.
#'
#' Note that the function works without degrees of visual angle, being supplied.
#' However, we strongly recommend that \code{dva_x} and \code{dva_y} are provided
#' (especially if saccades are of interest) so that velocities and amplitudes
#' can be returned in deg/s and deg, respectively (see
#' \link[=VisualAngle]{VisualAngle}).
#'
#' @author Martin R. Vasilev
#'
#' @param data A data frame containing the raw data samples.
#'  Must include at least the following columns: "time", "x", and "y". Note that time must be measured in milliseconds
#' @param dva_x Degrees of visual angle per pixel (x dimension); optional, but recommended
#'
#' @param dva_y Degrees of visual angle per pixel (y dimension); optional, but recommended
#'
#'
#' @param lambda Velocity threshold, expressed in multiples of standard deviation.
#'
#' @param min_fix_dur Minimum fixation duration (in ms) to extract
#'
#' @param return_saccades Logical. If \code{FALSE} (default), return fixations.
#' If \code{TRUE}, return saccades instead.
#'
#' @param min_sacc_amplitude: Minimum saccade amplitude in degrees (if using saccade detection). By default, this is set to 0.15.
#'
#' @return A data frame with one row per detected event. The columns depend on
#' whether fixations or saccades are returned.
#'
#'
#' If \code{return_saccades = FALSE}, the function returns fixation-level data
#' with the following columns:
#' \describe{
#'   \item{\code{fix_id}}{Sequential fixation identifier.}
#'   \item{\code{fix_start}}{Start time of the fixation, in the same units as \code{data$time}.}
#'   \item{\code{fix_end}}{End time of the fixation, in the same units as \code{data$time}.}
#'   \item{\code{x}}{Mean horizontal gaze position during the fixation, in pixels.}
#'   \item{\code{y}}{Mean vertical gaze position during the fixation, in pixels.}
#'   \item{\code{fix_dur}}{Fixation duration, computed as \code{fix_end - fix_start}.}
#'   \item{\code{avg_velocity}}{Mean sample-to-sample velocity during the fixation, in degrees/second.}
#'   \item{\code{n_samples}}{Number of samples contributing to the fixation.}
#' }
#'
#' If \code{return_saccades = TRUE}, the function returns saccade-level data
#' with the following columns:
#' \describe{
#'   \item{\code{sacc_id}}{Sequential event identifier for the detected saccade.}
#'   \item{\code{sacc_start}}{Start time of the saccade, in the same units as \code{data$time}.}
#'   \item{\code{sacc_end}}{End time of the saccade, in the same units as \code{data$time}.}
#'   \item{\code{sacc_dur}}{Saccade duration, computed as \code{sacc_end - sacc_start}.}
#'   \item{\code{x_start}}{Horizontal gaze position at saccade onset, in pixels.}
#'   \item{\code{y_start}}{Vertical gaze position at saccade onset, in pixels.}
#'   \item{\code{x_end}}{Horizontal gaze position at saccade offset, in pixels.}
#'   \item{\code{y_end}}{Vertical gaze position at saccade offset, in pixels.}
#'   \item{\code{sacc_amplitude_deg}}{Saccade amplitude in degrees of visual angle, computed from the horizontal and vertical displacement between saccade onset and offset.}
#'   \item{\code{avg_velocity_deg}}{Mean sample-to-sample velocity during the saccade, in degrees/second.}
#'   \item{\code{peak_velocity_deg}}{Maximum sample-to-sample velocity during the saccade, in degrees/second.}
#'   \item{\code{n_samples}}{Number of samples contributing to the saccade.}
#' }
#'
#' @references
#' Engbert, R., & Kliegl, R. (2003).
#' Microsaccades uncover the orientation of covert attention.
#' \emph{Vision Research, 43}(9), 1035--1045.
#' \doi{10.1016/S0042-6989(03)00084-1}
#'
#' @export

EngbertKliegl03 <- function(data,
                            lambda = 6,
                            min_fix_dur = 50,
                            return_saccades = FALSE,
                            min_sacc_amplitude = 0.15,
                            dva_x= NULL,
                            dva_y= NULL){

  # make sure data is ordered by time:
  data <- data[order(data$time), ]

  # check we have enough observations:
  n <- nrow(data)

  if (n < 2) {
    message("Not enough observations in dataset!")
    return(data.frame())
  }

  # check required variables are present:
  if (!all(c("time", "x", "y") %in% names(data))) {
    stop("The following variables are required for the analysis: 'time', 'x', and 'y'. Please make sure you use the exact same spelling.")
  }


  t <- data$time / 1000 # get time in seconds

  # get time difference for variable sampling rates:
  dt_var <- t[5:n] - t[1:(n - 4)]


  # use pixel values by default:
  x_px <- data$x
  y_px <- data$y

  x_pos= data$x # get x positions
  y_pos= data$y # get y positions


  # Use degrees only for amplitude and velocity if supplied
  if(!is.null(dva_x) & !is.null(dva_y)) {
    x_metric <- x_px * dva_x
    y_metric <- y_px * dva_y
    unit_amp <- "deg"
    unit_vel <- "deg/s"
  } else {
    x_metric <- x_px
    y_metric <- y_px
    unit_amp <- "px"
    unit_vel <- "px/s"
  }


  # initiate velocity vectors:
  vx <- rep(NA, n)
  vy <- rep(NA, n)

  # Calculate x-position vector:
  vx[3:(n - 2)] <- (
    x_metric[5:n] + x_metric[4:(n - 1)] -
      x_metric[2:(n - 3)] - x_metric[1:(n - 4)]
  ) / (1.5 * dt_var)

  vy[3:(n - 2)] <- (
    y_metric[5:n] + y_metric[4:(n - 1)] -
      y_metric[2:(n - 3)] - y_metric[1:(n - 4)]
  ) / (1.5 * dt_var)

  # because we are using 4 samples, 1.5*dt_var gives us the 6*dt in the formula
  # this accounts for (potentially) variable sampling rates

  speed <- sqrt(vx^2 + vy^2)


  # Robust median-based SD estimator
  msd <- function(v) {
    sqrt(median(v^2, na.rm = TRUE) - median(v, na.rm = TRUE)^2)
  }

  # Median SD estimate:
  sx <- msd(vx)
  sy <- msd(vy)

  # Detetction thresholds for x and y axes:
  eta_x <- lambda * sx
  eta_y <- lambda * sy

  # saccade candidates are those exceeding the ellipse:
  sac_candidate <- (vx / eta_x)^2 + (vy / eta_y)^2 > 1
  # replace NA values:
  sac_candidate[is.na(sac_candidate)]<- FALSE

  r <- rle(sac_candidate)

  # Convert runs to tibble
  runs <- tibble(
    is_saccade = r$values,
    length = r$lengths
  ) %>%
    mutate(
      end = cumsum(length),
      start = end - length + 1
    )%>%
    mutate(
    # Create vector of sample indices for each event
    segment = map2(start, end, seq),

    # add temporal information:
    start_time = data$time[start],
    end_time = data$time[end],
    duration = end_time - start_time,
    duration_samples = length,

    # add positional information:
    start_x = x_px[start],
    start_y = y_px[start],
    end_x = x_px[end],
    end_y = y_px[end],
    mean_x = map_dbl(segment, ~ mean(x_px[.x], na.rm = TRUE)),
    mean_y = map_dbl(segment, ~ mean(y_px[.x], na.rm = TRUE)),

    # add amplitude:
    amp_x = x_metric[end] - x_metric[start],
    amp_y = y_metric[end] - y_metric[start],
    amplitude = sqrt(amp_x^2 + amp_y^2),

    # add velocity metrics:
    peak_velocity = map_dbl(
      segment,
      ~ {
        v <- speed[.x]
        v <- v[!is.na(v)]

        if(length(v) == 0) {
          NA_real_
        } else {
          max(v)
        }
      }
    ),

    mean_velocity = map_dbl(
      segment,
      ~ {
        v <- speed[.x]
        v <- v[!is.na(v)]

        if(length(v) == 0) {
          NA_real_
        } else {
          mean(v)
        }
      }
    )
    )

  if(!return_saccades){ # if returning fixations:

    # select only fixation events and filter durations under the minimum:
    fix<- runs%>%
      dplyr::filter(is_saccade==F & duration>= min_fix_dur)

    fix<- fix%>%
      dplyr::transmute(fix_id = dplyr::row_number(),
                fix_start= start_time,
                fix_end= end_time,
                fix_dur= duration,
                x= mean_x,
                y= mean_y,
                avg_velocity= mean_velocity,
                n_samples= length
                )

    return(fix)

  }else{

    # return saccade data:
    sacc<- runs%>%
      dplyr::filter(is_saccade==T)

    # truncate saccades based on amplitude if degrees are provided:
    if(unit_amp=="deg"){
      sacc<-  sacc%>%
        dplyr::filter(amplitude>= min_sacc_amplitude)
    }

    sacc<- sacc%>%
      dplyr::transmute(sacc_id = dplyr::row_number(),
                sacc_start= start_time,
                sacc_end= end_time,
                sacc_dur= duration,
                x_start= start_x,
                y_start= start_y,
                x_end= end_x,
                y_end= end_y,
                sacc_amplitude= amplitude,
                avg_velocity= mean_velocity,
                peak_velocity= peak_velocity,
                n_samples= length
                )

    # Rename if degrees were used
    if(!is.null(dva_x) & !is.null(dva_y)) {

      sacc <- sacc %>%
        rename(
          sacc_amplitude_deg = sacc_amplitude,
          avg_velocity_deg = avg_velocity,
          peak_velocity_deg = peak_velocity
        )

    }

    return(sacc)


  }



}
