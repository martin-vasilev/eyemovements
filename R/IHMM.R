#' @importFrom magrittr %>%
NULL


#' Detect fixations or saccades using Identification by Hidden Markov Models
#'
#' Detects fixations or saccades from eye-tracking samples using a Hidden
#' Markov Model (HMM) approach inspired by the Identification by Hidden
#' Markov Models (I-HMM) method proposed by Salvucci and Goldberg (2000).
#'
#' The algorithm estimates hidden fixation and saccade states from
#' sample-to-sample gaze velocities using a Gaussian Hidden Markov Model
#' fitted with the \pkg{depmixS4} package. Velocities are calculated from
#' successive gaze samples and the Viterbi algorithm is used to infer the
#' most likely sequence of hidden states.
#'
#' By default, the function returns fixation data.
#' To return saccade data instead, set \code{return_saccades = TRUE}.
#'
#' Note that the function works without degrees of visual angle being supplied.
#' However, we strongly recommend that \code{dva_x} and \code{dva_y} are
#' provided (especially if saccades are of interest) so that velocities and
#' amplitudes can be returned in deg/s and deg, respectively (see
#' \link[=VisualAngle]{VisualAngle}).
#'
#' @author Martin R. Vasilev
#'
#' @param data A data frame containing the raw data samples.
#' Must include at least the following columns: \code{"time"},
#' \code{"x"}, and \code{"y"}. Time must be measured in milliseconds.
#'
#' @param dva_x Degrees of visual angle per pixel (x dimension);
#' optional, but recommended.
#'
#' @param dva_y Degrees of visual angle per pixel (y dimension);
#' optional, but recommended.
#'
#' @param return_saccades Logical. If \code{FALSE} (default), return
#' fixations. If \code{TRUE}, return saccades instead.
#'
#' @param min_fix_dur Minimum fixation duration (in ms) to extract.
#' Fixations shorter than this value are discarded.
#'
#' @param min_sacc_amplitude Minimum saccade amplitude in degrees
#' (if using saccade detection and degrees of visual angle are supplied).
#' By default, this is set to 0.15 deg.
#'
#' @param nstates Number of hidden states in the HMM.
#' By default, this is set to 2 (fixation and saccade states).
#'
#' @param return_samples A logical indicating whether to return raw samples (with states mapped). FALSE by default, can be set to TRUE to obtain the samples instead.
#'
#' @return A data frame with one row per detected event.
#' The columns depend on whether fixations or saccades are returned.
#'
#'
#' If \code{return_saccades = FALSE}, the function returns fixation-level data
#' with the following columns:
#'
#' \describe{
#'   \item{\code{fix_id}}{Sequential fixation identifier.}
#'   \item{\code{fix_start}}{Start time of the fixation, in the same units as \code{data$time}.}
#'   \item{\code{fix_end}}{End time of the fixation, in the same units as \code{data$time}.}
#'   \item{\code{fix_dur}}{Fixation duration, computed as \code{fix_end - fix_start}.}
#'   \item{\code{x}}{Mean horizontal gaze position during the fixation, in pixels.}
#'   \item{\code{y}}{Mean vertical gaze position during the fixation, in pixels.}
#'   \item{\code{avg_velocity}}{Mean sample-to-sample velocity during the fixation.}
#'   \item{\code{n_samples}}{Number of samples contributing to the fixation.}
#' }
#'
#' If \code{return_saccades = TRUE}, the function returns saccade-level data
#' with the following columns:
#'
#' \describe{
#'   \item{\code{sacc_id}}{Sequential event identifier for the detected saccade.}
#'   \item{\code{sacc_start}}{Start time of the saccade, in the same units as \code{data$time}.}
#'   \item{\code{sacc_end}}{End time of the saccade, in the same units as \code{data$time}.}
#'   \item{\code{sacc_dur}}{Saccade duration, computed as \code{sacc_end - sacc_start}.}
#'   \item{\code{x_start}}{Horizontal gaze position at saccade onset, in pixels.}
#'   \item{\code{y_start}}{Vertical gaze position at saccade onset, in pixels.}
#'   \item{\code{x_end}}{Horizontal gaze position at saccade offset, in pixels.}
#'   \item{\code{y_end}}{Vertical gaze position at saccade offset, in pixels.}
#'   \item{\code{sacc_amplitude_deg}}{Saccade amplitude in degrees of visual angle (if degrees of visual angle are supplied).}
#'   \item{\code{avg_velocity_deg}}{Mean sample-to-sample velocity during the saccade, in degrees/second (if degrees of visual angle are supplied).}
#'   \item{\code{peak_velocity_deg}}{Maximum sample-to-sample velocity during the saccade, in degrees/second (if degrees of visual angle are supplied).}
#'   \item{\code{n_samples}}{Number of samples contributing to the saccade.}
#' }
#'
#' @references
#' Salvucci, D. D., & Goldberg, J. H. (2000).
#' Identifying fixations and saccades in eye-tracking protocols.
#' \emph{Proceedings of the 2000 Symposium on Eye Tracking Research &
#' Applications}, 71--78.
#'
#' @seealso
#' \link[=IVT]{IVT},
#' \link[=IDT]{IDT},
#' \link[=EngbertKliegl03]{EngbertKliegl03},
#' \link[=VisualAngle]{VisualAngle}
#'
#' @export
IHMM <- function(data,
                 dva_x = NULL,
                 dva_y = NULL,
                 return_saccades = FALSE,
                 min_fix_dur = 50,
                 min_sacc_amplitude = 0.15,
                 nstates = 2,
                 return_samples= FALSE){

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

  if(!is.numeric(data$time) | !is.numeric(data$x) | !is.numeric(data$y)) {
    stop("'time', 'x', and 'y' must be numeric.")
  }


  t <- data$time / 1000

  x_px <- data$x
  y_px <- data$y

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

  dt <- c(NA, diff(t))

  vx <- c(NA, diff(x_metric)) / dt
  vy <- c(NA, diff(y_metric)) / dt

  speed <- sqrt(vx^2 + vy^2)
  speed[!is.finite(speed)] <- NA

  # Fit HMM:
  hmm_dat <- data.frame(
    speed = speed
  )

  # Remove rows with NAs:
  valid <- !is.na(hmm_dat$speed)

  hmm_fit_dat <- hmm_dat[valid, , drop = FALSE]

  mod <- depmixS4::depmix(
    response = speed ~ 1,
    data = hmm_fit_dat,
    nstates = nstates,
    family = gaussian(),
    ntimes = nrow(hmm_fit_dat)
  )

  # Fit & Extract model:
  fit_mod <- depmixS4::fit(mod, verbose = FALSE)
  post <- depmixS4::posterior(fit_mod, type = "viterbi")

  decoded_state <- rep(NA, n)
  decoded_state[valid] <- post$state

  # Identify which state is the saccade state.
  # The state with the larger mean speed is treated as saccade.
  state_means <- tapply(hmm_fit_dat$speed, post$state, mean, na.rm = TRUE)
  saccade_state <- as.integer(names(which.max(state_means)))

  is_saccade <- decoded_state == saccade_state
  is_saccade[is.na(is_saccade)] <- FALSE

  if(return_samples) {
    sample_states <- data %>%
      dplyr::mutate(
        speed = speed,
        state = ifelse(is_saccade, "Saccade", "Fixation")
      )

    return(sample_states)
  }

  r <- rle(is_saccade)

  runs <- tibble::tibble(
    is_saccade = r$values,
    length = r$lengths
  ) %>%
    dplyr::mutate(
      end = cumsum(length),
      start = end - length + 1,
      segment = purrr::map2(start, end, seq),

      start_time = data$time[start],
      end_time = data$time[end],
      duration = end_time - start_time,
      duration_samples = length,

      start_x = x_px[start],
      start_y = y_px[start],
      end_x = x_px[end],
      end_y = y_px[end],

      mean_x = purrr::map_dbl(segment, ~ mean(x_px[.x], na.rm = TRUE)),
      mean_y = purrr::map_dbl(segment, ~ mean(y_px[.x], na.rm = TRUE)),

      amp_x = x_metric[end] - x_metric[start],
      amp_y = y_metric[end] - y_metric[start],
      amplitude = sqrt(amp_x^2 + amp_y^2),

      peak_velocity = purrr::map_dbl(segment, ~ {
        v <- speed[.x]
        v <- v[!is.na(v)]
        if (length(v) == 0) NA_real_ else max(v)
      }),

      mean_velocity = purrr::map_dbl(segment, ~ {
        v <- speed[.x]
        v <- v[!is.na(v)]
        if (length(v) == 0) NA_real_ else mean(v)
      })
    )

  if (!return_saccades) {

    fix <- runs %>%
      dplyr::filter(!is_saccade)

    if (!is.null(min_fix_dur)) {
      fix <- fix %>%
        dplyr::filter(duration >= min_fix_dur)
    }

    fix <- fix %>%
      dplyr::transmute(
        fix_id = dplyr::row_number(),
        fix_start = start_time,
        fix_end = end_time,
        fix_dur = duration,
        x = mean_x,
        y = mean_y,
        avg_velocity = mean_velocity,
        n_samples = duration_samples
      )

    return(fix)
  } else{

    sacc <- runs %>%
      dplyr::filter(is_saccade)

    if (unit_amp == "deg" && !is.null(min_sacc_amplitude)) {
      sacc <- sacc %>%
        dplyr::filter(amplitude >= min_sacc_amplitude)
    }

    sacc <- sacc %>%
      dplyr::transmute(
        sacc_id = dplyr::row_number(),
        sacc_start = start_time,
        sacc_end = end_time,
        sacc_dur = duration,
        x_start = start_x,
        y_start = start_y,
        x_end = end_x,
        y_end = end_y,
        sacc_amplitude = amplitude,
        avg_velocity = mean_velocity,
        peak_velocity = peak_velocity,
        n_samples = duration_samples
      )

    if (unit_amp == "deg") {
      sacc <- sacc %>%
        dplyr::rename(
          sacc_amplitude_deg = sacc_amplitude,
          avg_velocity_deg = avg_velocity,
          peak_velocity_deg = peak_velocity
        )
    }

    return(sacc)


  }



}

