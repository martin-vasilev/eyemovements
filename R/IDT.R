#' @importFrom magrittr %>%
NULL

#' Detect fixations or saccades from eye samples using the Identification by Dispersion Threshold  (I-DT) method
#'
#'This function detects fixations from raw eye position samples
#'using the Identification by Dispersion Threshold  (I-DT). Note that the algorithm
#'requires calculations based on visual angle (see
#' \link[=VisualAngle]{VisualAngle}). As a result, the degrees of visual
#' angle per pixel need to be provided for the x- and y-dimensions (
#'\code{dva_x} and \code{dva_y}).
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
#' @param window_threshold Minimum fixation duration window threshold in milliseconds (I-DT algorithm)
#'
#' @param disp_threshold Minimum sample dispersion threshold in visual angle (I-DT algorithm)
#'
#' @param min_sacc_amplitude: Minimum saccade amplitude in degrees (if using saccade detection). By default, this is set to 0.15.
#'
#' @param return_saccades Logical. If \code{FALSE} (default), return fixations.
#' If \code{TRUE}, return saccades instead.
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
#' @export

IDT <- function(data, dva_x = 0.0187,
                      dva_y = 0.0192,
                      window_threshold = 50,
                      disp_threshold = 1,
                      return_saccades = FALSE,
                      min_sacc_amplitude= 0.15
                ){

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


  ####### I-DT #######

  # function to calculate dispersion in degres:
  dispersion_deg <- function(start, end) {
    dx <- (max(data$x[start:end], na.rm=TRUE) - min(data$x[start:end], na.rm=TRUE)) * dva_x
    dy <- (max(data$y[start:end], na.rm=TRUE) - min(data$y[start:end], na.rm=TRUE)) * dva_y

    return(dx + dy)
  }

  # initialise variables:
  start <- 1
  end<- NULL
  fix_id <- 1
  fix <- data.frame()


  while(start <= n){

    start_time <- data$time[start] # start time of 1st sample

    # find end time based on interval window
    end <- findInterval(start_time + window_threshold, data$time)

    # If there isn't enough time for window
    if(end <= start){
      break # exit loop
    }

    # calculate dispersion for current window:
    D <- dispersion_deg(start, end)



    if(D <= disp_threshold) { # if threshold not met

      # expand until dispersion exceeds threshold
      last_good_end <- end

      while (end < n) {
        end <- end + 1 # increment window by 1 point
        D <- dispersion_deg(start, end) # recalculate dispersion

        if(D > disp_threshold){
          break # stop loop, we have found window's end
        }

        last_good_end <- end
      }

      # Save fixation:
      fix <- rbind(
        fix,
        data.frame(
          'fix_id' = fix_id,
          'fix_start'   = data$time[start],
          'fix_end'   = data$time[last_good_end],
          'x'      = mean(data$x[start:last_good_end], na.rm=TRUE),
          'y'      = mean(data$y[start:last_good_end], na.rm=TRUE),
          'fix_dur' = data$time[last_good_end] - data$time[start],
          'fix_dispersion_deg' = dispersion_deg(start, last_good_end)#,
         # 'n_samples'  = last_good_end - start+1
        ))

      # advance start and fix_id:
      fix_id <- fix_id + 1
      start  <- last_good_end + 1


    } else{
      # If initial window is too dispersed,
      # drop the first point and retry:

      start <- start + 1 # increment start by 1 sample
      next # go to next iteration

    }

  } # end of while loop


  # If returning fixations:
  if(!return_saccades){
    return(fix)

  }else{
    # calculate and return saccades:

    # Saccades can be taken as intervals between fixations:

    sacc <- fix %>%
      dplyr::arrange(fix_start) %>%
      dplyr::mutate(
        next_fix_start = dplyr::lead(fix_start),
        next_x = dplyr::lead(x),
        next_y = dplyr::lead(y)
      ) %>%
      dplyr::filter(!is.na(next_fix_start)) %>%
      dplyr::transmute(
        sacc_id = dplyr::row_number(),
        sacc_start = fix_end,
        sacc_end = next_fix_start,
        sacc_dur = sacc_end - sacc_start,
        x_start = x,
        y_start = y,
        x_end = next_x,
        y_end = next_y,
        amp_x_deg = (x_end - x_start) * dva_x,
        amp_y_deg = (y_end - y_start) * dva_y,
        sacc_amplitude_deg = sqrt(amp_x_deg^2 + amp_y_deg^2)
      )

    if(length(min_sacc_amplitude)>0){

      sacc<- subset(sacc, sacc_amplitude_deg>= min_sacc_amplitude)
      sacc<- sacc%>% dplyr::mutate(sacc_id = dplyr::row_number())

    }

    sacc<- sacc%>% dplyr::select(-c(amp_x_deg, amp_y_deg))

    # calculate sample-to-sample velocities and add to data frame:
    data_vel <- data %>%
      dplyr::arrange(time) %>%
      dplyr::mutate(
        dx_deg = (x - dplyr::lag(x)) * dva_x,
        dy_deg = (y - dplyr::lag(y)) * dva_y,
        dt_sec = (time - dplyr::lag(time)) / 1000,  # time in ms

        velocity_deg_s = sqrt(dx_deg^2 + dy_deg^2) / dt_sec
      )

    sacc <- sacc %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        avg_velocity_deg = mean(
          data_vel$velocity_deg_s[
            data_vel$time >= sacc_start & data_vel$time <= sacc_end
          ],
          na.rm = TRUE
        ),

        peak_velocity_deg = max(
          data_vel$velocity_deg_s[
            data_vel$time >= sacc_start & data_vel$time <= sacc_end
          ],
          na.rm = TRUE
        )
      ) %>%
      dplyr::ungroup()

    return(sacc)

  } # end of return_saccades


} # end of function
