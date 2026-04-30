#' @importFrom magrittr %>%
NULL


#' Detect fixations or saccades from eye samples using the Identification by Velocity Threshold  (I-VT) method
#'
#'This function detects fixations or saccades from raw eye position samples
#'using the I-VT algortiythm. Most algorithms
#'require calculations based on visual angle (see
#' \link[=VisualAngle]{VisualAngle}). As a result, the degrees of visual
#' angle per pixel need to be provided for the x- and y-dimensions (
#'\code{dva_x} and \code{dva_y}). By default, the function returns fixation data.
#'To return saccade data, set `return_saccades= T`.
#'
#' @author Martin R. Vasilev, Yixin Ding
#'
#' @param data A data frame containing the raw data samples.
#'  Must include at least the following columns: "time", "x", and "y".
#'
#' @param dva_x Degrees of visual angle per pixel (x dimension)
#'
#' @param dva_y Degrees of visual angle per pixel (y dimension)
#'
#' @param vel_threshold Velocity threshold in degrees/second (I-VT algorithm).
#'
#' @param min_fix_dur Minimum fixation duration (in ms) to extract
#'
#' @param return_saccades Logical. If \code{FALSE} (default), return fixations.
#' If \code{TRUE}, return saccades instead.
#'
#' @param min_sac_samples Minimum number of samples required to classify a saccade (I-VT).
#' Used to remove spurious 1-sample velocity spikes.
#'
#' @return A data frame with one row per detected event. The columns depend on
#' whether fixations or saccades are returned.
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
#' @export

IVT <- function(data, dva_x = 0.0187,
                            dva_y = 0.0192,
                            vel_threshold = 30,
                            min_fix_dur = 50,
                            return_saccades = FALSE,
                            min_sacc_amplitude = 0.15){

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
    stop("The following variables are required for the analysis: 'time', 'x', and 'y'. Please make sure you use the exact same naming.")
  }


  ####### I-VT #######

  t <- data$time / 1000 # get time in seconds
  dt <- c(NA, diff(t)) # take sample time differences

  # calculate sample deviations in visual angle:
  dx_deg <- c(NA, diff(data$x) * dva_x)
  dy_deg <- c(NA, diff(data$y) * dva_y)

  # calculate velocity magnitude:
  vel <- rep(NA, n) # initiate vector

  # protect from missing and duplicate values:
  valid <- !is.na(dt) & dt > 0

  # calculate velocities
  vel[valid] <- sqrt((dx_deg[valid] / dt[valid])^2 + (dy_deg[valid] / dt[valid])^2)

  # check if velocity threshold in deg/s is met:
  is_saccade <- !is.na(vel) & (vel >= vel_threshold)

  # save data in temporarey data frame for processing:
  tmp <- data.frame(
    time = data$time,
    x = data$x,
    y = data$y,
    dx_deg = dx_deg,
    dy_deg = dy_deg,
    vel = vel,
    is_saccade = is_saccade
  )

  if(!return_saccades){
    # identify fixation sequences as adjacent non-saccades:
    tmp2 <- tmp %>%
      dplyr::mutate(
        # a new fixation starts when we enter a non-saccade run
        fixation_group = cumsum(
          (!is_saccade) & dplyr::lag(is_saccade, default = TRUE)
        )
      ) %>%
      dplyr::filter(!is_saccade, fixation_group > 0)

    # Now summarise into fixations:
    fix <- tmp2 %>%
      dplyr::group_by(fixation_group) %>%
      dplyr::summarise(
        fix_start = min(time, na.rm = TRUE),
        fix_end   = max(time, na.rm = TRUE),
        fix_dur   = fix_end - fix_start,
        x         = mean(x, na.rm = TRUE),
        y         = mean(y, na.rm = TRUE),
        n_samples = dplyr::n(),
        avg_velocity  = mean(vel, na.rm=TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(fix_id = dplyr::row_number()) %>%
      dplyr::select(fix_id, fix_start, fix_end, x, y, fix_dur, avg_velocity, n_samples)

    # if minimum fixation duration is specified, subset the data:
    if(length(min_fix_dur)>0){

      fix<- subset(fix, fix_dur>= min_fix_dur)
      fix<- fix%>% dplyr::mutate(fix_id = dplyr::row_number())

    }
    return(fix)
  }else{

    # identify saccade sequences as adjacent non-fixations:
    tmp2 <- tmp %>%
      dplyr::mutate(
        # a new fixation starts when we enter a non-saccade run
        fixation_group = cumsum(
          (is_saccade) & dplyr::lag(!is_saccade, default = TRUE)
        )
      ) %>%
      dplyr::filter(is_saccade, fixation_group > 0)

    # Now summarise into saccades:
    sacc <- tmp2 %>%
      dplyr::group_by(fixation_group) %>%
      dplyr::summarise(
        sacc_start    = min(time, na.rm = TRUE),
        sacc_end      = max(time, na.rm = TRUE),
        sacc_dur      = sacc_end - sacc_start,
        x_start = dplyr::first(x),
        y_start = dplyr::first(y),
        x_end   = dplyr::last(x),
        y_end   = dplyr::last(y),
        n_samples     = dplyr::n(),
        avg_velocity_deg  = mean(vel, na.rm=TRUE),
        peak_velocity_deg = max(vel, na.rm=TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(amp_x_deg = (x_end - x_start) * dva_x,
                    amp_y_deg = (y_end - y_start) * dva_y,
                    sacc_amplitude_deg = sqrt(amp_x_deg^2 + amp_y_deg^2),
                    sacc_id = dplyr::row_number()) %>%
      dplyr::select(sacc_id, sacc_start, sacc_end, sacc_dur,
                    x_start, y_start, x_end, y_end, sacc_amplitude_deg,
                    avg_velocity_deg, peak_velocity_deg, n_samples)

    if(length(min_sacc_amplitude)>0){

      sacc<- subset(sacc, sacc_amplitude_deg>= min_sacc_amplitude)
      sacc<- sacc%>% dplyr::mutate(sacc_id = dplyr::row_number())

    }



  }



}
