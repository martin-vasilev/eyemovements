
#' @importFrom magrittr %>%
NULL

#' Detect fixations from eye samples
#'
#'This function detects fixations from raw eye position samples
#'using a number of possible detection algorithms. Most algorithms
#'require calculations based on visual angle (see
#' \link[=VisualAngle]{VisualAngle}). As a result, the degrees of visual
#' angle per pixel need to be provided for the x- and y-dimensions (
#'\code{dva_x} and \code{dva_y}).
#' The function currently supports the following detection methods:
#' - Identification by Dispersion Threshold  (I-DT)
#'
#' @author Martin R. Vasilev, Yixin Ding
#'
#' @param data A data frame containing the raw data samples.
#'  Must include at least the following columns: "time", "x", and "y".
#'
#' @param method The fixation detection algorithm to use (I-DT or I-VT)
#'
#' @param dva_x Degrees of visual angle per pixel (x dimension)
#'
#' @param dva_y Degrees of visual angle per pixel (y dimension)
#'
#' @param window_threshold Minimum fixation duration window threshold in milliseconds (I-DT algorithm)
#'
#' @param disp_threshold Minimum sample disperson threshold in visual angle (I-DT algorithm)
#'
#' @param vel_threshold Velocity threshold in degrees/second (I-VT algorithm).
#'
#' @param min_fix_dur Minimum fixation duration (in ms) to extract
#'
#' @return A data frame with one row per detected fixation and the following columns:
#' \describe{
#'   \item{fix_id}{Sequential fixation identifier within the trial.}
#'   \item{fix_start}{Start time of the fixation (in milliseconds, eye-tracker internal clock).}
#'   \item{fix_end}{End time of the fixation (in milliseconds).}
#'   \item{x}{Mean horizontal gaze position of all samples in the fixation (in pixels).}
#'   \item{y}{Mean vertical gaze position of all samples in the fixation (in pixels).}
#'   \item{fix_dur}{Fixation duration in milliseconds, computed as
#'   \code{end_fix - start_fix}.}
#'   \item{dispersion_deg}{Total spatial dispersion of samples in the fixation,
#'   computed as the sum of horizontal and vertical dispersion in degrees of visual angle
#'   (I-DT algorithm).}
#'   \item{velocity}{ Average sample-to-sample velocity within the fixation in degrees per second (I-VT algorithm)

#'   \item{n_samples}{Number of raw eye-tracking samples contributing to the fixation.}
#' }

#'
#' @examples
#' data("data_Oz")
#'
#'# I-DT method:
#'
#' fixations <- DetectFixations(
#'   data = data_Oz, method = "I-DT",
#'   dva_x = 0.0187, dva_y = 0.0192,
#'   window_threshold = 100, disp_threshold = 1
#' )
#'
#' head(fixations)
#'
#'# I-VT method:
#'
#' fixations <- DetectFixations(
#'   data = data_Oz, method = "I-VT",
#'   dva_x = 0.0187, dva_y = 0.0192,
#'   vel_threshold = 40, min_fix_dur = 50
#' )
#'
#' head(fixations)
#' @source Salvucci, D. D., & Goldberg, J. H. (2000, November). Identifying fixations and saccades in eye-tracking protocols. In Proceedings of the 2000 symposium on Eye tracking research & applications (pp. 71-78).
#' @export


DetectFixations<- function(data, method= "I-VT", dva_x= 0.0187,
                           dva_y= 0.0192, window_threshold = 100,
                           disp_threshold = 1,
                           vel_threshold= 40,
                           min_fix_dur= 50){

  # make sure data is ordered by time:
  data <- data[order(data$time), ]

  # check we have enough observations:
  n<- nrow(data)

  if (n < 2){

    message('Not enough observations in dataset!')
    return(data.frame())

  }

  fix <- NULL

####### I-DT: #######
  if(method== "I-DT"){

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
            'dispersion_deg' = dispersion_deg(start, last_good_end),
            'n_samples'  = last_good_end - start+1
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

    # end of I-DT algorithm

  }else{

    if(method== "I-VT"){

      ### IV-T:

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
          velocity  = mean(vel, na.rm=TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(fix_id = dplyr::row_number()) %>%
        dplyr::select(fix_id, fix_start, fix_end, x, y, fix_dur, velocity, n_samples)

        # if minimum fixation duration is specified, subset the data:
        if(length(min_fix_dur)>0){

          fix<- subset(fix, fix_dur>= min_fix_dur)

        }































    }else{

      stop(sprintf('Fixation detection method "%s" not recognised!\n Available methods are: I-DT and I-VT.', method))

    }

  }



  return(fix)




}

