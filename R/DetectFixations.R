
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
#' @param method The fixation detection algorithm to use (I-DT)
#'
#' @param dva_x Degrees of visual angle per pixel (x dimension)
#'
#' @param dva_y Degrees of visual angle per pixel (y dimension)
#'
#' @param window_threshold Minimum fixation duration window threshold in milliseconds (I-DT algorithm)
#'
#' @param disp_threshold Minimum sample disperson threshold in visual angle (I-DT algorithm)
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
#'   \item{n_samples}{Number of raw eye-tracking samples contributing to the fixation.}
#' }

#'
#' @examples
#' data("data_Oz")
#'
#' fixations <- DetectFixations(
#'   data = data_Oz, method = "I-DT",
#'   dva_x = 0.0187, dva_y = 0.0192,
#'   window_threshold = 100, disp_threshold = 1
#' )
#'
#' head(fixations)
#' @source Salvucci, D. D., & Goldberg, J. H. (2000, November). Identifying fixations and saccades in eye-tracking protocols. In Proceedings of the 2000 symposium on Eye tracking research & applications (pp. 71-78).
#' @export


DetectFixations<- function(data, method= "I-DT", dva_x= 0.0187,
                           dva_y= 0.0192, window_threshold = 100,
                           disp_threshold = 1){

  # make sure data is ordered by time:
  data <- data[order(data$time), ]

  # check we have enough observations:
  n<- nrow(data)

  if (n < 2){

    message('Not enough observations in dataset!')
    return(data.frame())

  }

  ### I-DT:

  # function to calculate dispersion in degres:
  dispersion_deg <- function(start, end) {
    dx <- (max(data$x[start:end], na.rm=TRUE) - min(data$x[start:end], na.rm=TRUE)) * dva_x
    dy <- (max(data$y[start:end], na.rm=TRUE) - min(data$y[start:end], na.rm=TRUE)) * dva_y

    return(dx + dy)
  }

  # initialise variables:
  fix <- NULL
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






  } # end of I-DT loop

  return(fix)




}

