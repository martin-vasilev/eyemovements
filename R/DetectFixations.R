
#' Detect fixations or saccades from eye samples
#'
#'This function detects fixations from raw eye position samples
#'using a number of possible detection algorithms. Most algorithms
#'require calculations based on visual angle (see
#' \link[=VisualAngle]{VisualAngle}). As a result, the degrees of visual
#' angle per pixel need to be provided for the x- and y-dimensions (
#'\code{dva_x} and \code{dva_y}).
#' The function currently supports the following detection methods:
#' - Identification by Dispersion Threshold  (I-DT)
#' - Identification by Velocity Threshold (I-VT)
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
#' @param return_saccades Logical. If \code{FALSE} (default), return fixations.
#' If \code{TRUE}, return saccades instead.
#'
#' @param min_sac_samples Minimum number of samples required to classify a saccade (I-VT).
#' Used to remove spurious 1-sample velocity spikes.
#'
#' @return A data frame with one row per detected event.
#' If \code{return_saccades = FALSE}, the output contains fixation-level data.
#' If \code{return_saccades = TRUE}, the output contains saccade-level data.
#'
#' @export
DetectFixations <- function(data, method = "I-VT", dva_x = 0.0187,
                            dva_y = 0.0192, window_threshold = 100,
                            disp_threshold = 1,
                            vel_threshold = 40,
                            min_fix_dur = 50,
                            lambda = 6,
                            return_saccades = FALSE,
                            min_sac_samples = 2){

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

  # helper to summarise contiguous runs
  summarise_runs <- function(tmp, run_var, keep_value,
                             id_name, start_name, end_name, dur_name,
                             extra_col_name = NULL, extra_values = NULL) {

    tmp2 <- tmp |>
      dplyr::mutate(
        run_group = cumsum(
          (!!as.name(run_var) == keep_value) &
            dplyr::lag((!!as.name(run_var)) != keep_value, default = TRUE)
        )
      ) |>
      dplyr::filter((!!as.name(run_var)) == keep_value, run_group > 0)

    if (nrow(tmp2) == 0) {
      out <- data.frame(
        id = numeric(0),
        start = numeric(0),
        end = numeric(0),
        x = numeric(0),
        y = numeric(0),
        dur = numeric(0),
        n_samples = numeric(0)
      )
      names(out) <- c(id_name, start_name, end_name, "x", "y", dur_name, "n_samples")
      return(out)
    }

    out <- tmp2 |>
      dplyr::group_by(run_group) |>
      dplyr::summarise(
        start = min(time, na.rm = TRUE),
        end   = max(time, na.rm = TRUE),
        dur   = end - start,
        x     = mean(x, na.rm = TRUE),
        y     = mean(y, na.rm = TRUE),
        n_samples = dplyr::n(),
        .groups = "drop"
      )

    if (!is.null(extra_col_name) && !is.null(extra_values) && extra_col_name %in% names(tmp2)) {
      extra_summary <- tmp2 |>
        dplyr::group_by(run_group) |>
        dplyr::summarise(
          extra = mean(.data[[extra_col_name]], na.rm = TRUE),
          .groups = "drop"
        )

      out <- dplyr::left_join(out, extra_summary, by = "run_group")
      names(out)[names(out) == "extra"] <- extra_values
    }

    out <- out |>
      dplyr::mutate(id = dplyr::row_number())

    names(out)[names(out) == "id"] <- id_name
    names(out)[names(out) == "start"] <- start_name
    names(out)[names(out) == "end"] <- end_name
    names(out)[names(out) == "dur"] <- dur_name

    out
  }

  ####### I-DT #######
  if (method == "I-DT") {

    # initialise fixation output
    fix <- data.frame(
      fix_id = numeric(0),
      fix_start = numeric(0),
      fix_end = numeric(0),
      x = numeric(0),
      y = numeric(0),
      fix_dur = numeric(0),
      dispersion_deg = numeric(0),
      n_samples = numeric(0),
      start_idx = numeric(0),
      end_idx = numeric(0)
    )

    # function to calculate dispersion in degrees:
    dispersion_deg_fun <- function(start, end) {
      dx <- (max(data$x[start:end], na.rm = TRUE) - min(data$x[start:end], na.rm = TRUE)) * dva_x
      dy <- (max(data$y[start:end], na.rm = TRUE) - min(data$y[start:end], na.rm = TRUE)) * dva_y
      dx + dy
    }

    start <- 1
    fix_id <- 1

    while (start <= n) {

      start_time <- data$time[start]
      end <- findInterval(start_time + window_threshold, data$time)

      if (end <= start) {
        break
      }

      D <- dispersion_deg_fun(start, end)

      if (D <= disp_threshold) {

        last_good_end <- end

        while (end < n) {
          end <- end + 1
          D <- dispersion_deg_fun(start, end)

          if (D > disp_threshold) {
            break
          }

          last_good_end <- end
        }

        fix <- rbind(
          fix,
          data.frame(
            fix_id = fix_id,
            fix_start = data$time[start],
            fix_end = data$time[last_good_end],
            x = mean(data$x[start:last_good_end], na.rm = TRUE),
            y = mean(data$y[start:last_good_end], na.rm = TRUE),
            fix_dur = data$time[last_good_end] - data$time[start],
            dispersion_deg = dispersion_deg_fun(start, last_good_end),
            n_samples = last_good_end - start + 1,
            start_idx = start,
            end_idx = last_good_end
          )
        )

        fix_id <- fix_id + 1
        start <- last_good_end + 1

      } else {
        start <- start + 1
        next
      }
    }

    # apply minimum duration to fixations if requested
    if (length(min_fix_dur) > 0 && nrow(fix) > 0) {
      fix <- subset(fix, fix_dur >= min_fix_dur)
      if (nrow(fix) > 0) {
        fix$fix_id <- seq_len(nrow(fix))
      }
    }

    if (!return_saccades) {
      fix <- fix[, c("fix_id", "fix_start", "fix_end", "x", "y",
                     "fix_dur", "dispersion_deg", "n_samples")]
      return(fix)
    }

    # derive saccades as contiguous runs of samples not assigned to fixations
    in_fix <- rep(FALSE, n)

    if (nrow(fix) > 0) {
      for (i in seq_len(nrow(fix))) {
        in_fix[fix$start_idx[i]:fix$end_idx[i]] <- TRUE
      }
    }

    tmp <- data.frame(
      time = data$time,
      x = data$x,
      y = data$y,
      is_saccade = !in_fix
    )

    sac <- summarise_runs(
      tmp = tmp,
      run_var = "is_saccade",
      keep_value = TRUE,
      id_name = "sac_id",
      start_name = "sac_start",
      end_name = "sac_end",
      dur_name = "sac_dur"
    )

    return(sac)
  }

  ####### I-VT #######
  if (method == "I-VT") {

    t <- data$time / 1000
    dt <- c(NA, diff(t))

    dx_deg <- c(NA, diff(data$x) * dva_x)
    dy_deg <- c(NA, diff(data$y) * dva_y)

    vel <- rep(NA, n)
    valid <- !is.na(dt) & dt > 0

    vel[valid] <- sqrt((dx_deg[valid] / dt[valid])^2 + (dy_deg[valid] / dt[valid])^2)

    is_saccade <- !is.na(vel) & (vel >= vel_threshold)

    tmp <- data.frame(
      time = data$time,
      x = data$x,
      y = data$y,
      vel = vel,
      is_saccade = is_saccade
    )

    if (!return_saccades) {
      fix <- tmp |>
        dplyr::mutate(
          fixation_group = cumsum((!is_saccade) & dplyr::lag(is_saccade, default = TRUE))
        ) |>
        dplyr::filter(!is_saccade, fixation_group > 0) |>
        dplyr::group_by(fixation_group) |>
        dplyr::summarise(
          fix_start = min(time, na.rm = TRUE),
          fix_end   = max(time, na.rm = TRUE),
          fix_dur   = fix_end - fix_start,
          x         = mean(x, na.rm = TRUE),
          y         = mean(y, na.rm = TRUE),
          n_samples = dplyr::n(),
          avg_velocity  = mean(vel, na.rm = TRUE),
          peak_velocity  = max(vel, na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::filter(n_samples >= min_sac_samples) %>%
        dplyr::mutate(sac_id = dplyr::row_number()) %>%
        dplyr::select(sac_id, sac_start, sac_end, x, y,
                      sac_dur, avg_velocity, peak_velocity, n_samples)

      if (length(min_fix_dur) > 0 && nrow(fix) > 0) {
        fix <- subset(fix, fix_dur >= min_fix_dur)
        if (nrow(fix) > 0) {
          fix <- dplyr::mutate(fix, fix_id = dplyr::row_number())
        }
      }

      return(fix)
    }

    sac <- tmp |>
      dplyr::mutate(
        saccade_group = cumsum(is_saccade & dplyr::lag(!is_saccade, default = TRUE))
      ) |>
      dplyr::filter(is_saccade, saccade_group > 0) |>
      dplyr::group_by(saccade_group) |>
      dplyr::summarise(
        sac_start = min(time, na.rm = TRUE),
        sac_end   = max(time, na.rm = TRUE),
        sac_dur   = sac_end - sac_start,
        x         = mean(x, na.rm = TRUE),
        y         = mean(y, na.rm = TRUE),
        n_samples = dplyr::n(),
        avg_velocity= mean(vel, na.rm = TRUE),
        peak_velocity= max(vel, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(sac_id = dplyr::row_number()) |>
      dplyr::select(sac_id, sac_start, sac_end, x, y, sac_dur, avg_velocity, peak_velocity, n_samples)

    return(sac)
  }

  if (method == "EK") {
    stop("The 'EK' branch is not yet implemented in this version.")
  }

  stop(sprintf(
    'Fixation detection method "%s" not recognised!\n Available methods are: I-DT and I-VT.',
    method
  ))
}
