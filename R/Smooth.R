
#' Smooth eye-tracking samples using different methods
#'
#' This function performs smoothing of eye samples prior to event detection to reduce noise. Samples can be smoothed using different methods, such as moving average, moving median, Savitzky-Golay filter.
#'
#' @param data A data frame of eye sample to smooth. Must include at least the following columns: "x", and "y".
#' @param method Data smooting method. Possible options are: "Mean", "Median", "SG" for Savitzky-Golay filter.
#' @param window_size Moving window size (number of samples) for moving average/ median methods. Must be a positive odd integer.
#' @param align Alignment of the window: "center" (default), "left", or "right".
#' @param p Savitzky-Golay filter order (default= 5)
#' @param n Savitzky-Golay filter length (must be odd, default= 23).
#' @return A numeric vector of the same length as \code{data}, containing the smoothed values.
#'
#' @examples
#' Oz_smooth <- SmoothSamples(data_Oz, method= "Mean")
#' Oz_smooth <- SmoothSamples(data_Oz, method= "Median")
#'
#' @export
SmoothSamples <- function(data, method= "Mean",
                window_size=5, align = "center", p=5, n=23) {

  if (!is.numeric(data$x)){
    stop("x must be numeric")
  }

  if (!is.numeric(data$y)){
    stop("y must be numeric")
  }

  if (window_size <= 0 || window_size %% 1 != 0) {
    stop("smooth_window must be a positive integer.")
  }

  if (align == "center" && window_size %% 2 == 0) {
    stop("k must be odd when align = 'center'")
  }

  ## Mean filter:
  if(method== "Mean"){

    # apply to x positions:
    data$x <- zoo::rollapply(
      data$x,
      width = window_size,
      FUN = function(z) mean(z, na.rm = TRUE),
      align = "center",
      fill = NA
    )

    # apply to y positions:
    data$y <- zoo::rollapply(
      data$y,
      width = window_size,
      FUN = function(z) mean(z, na.rm = TRUE),
      align = "center",
      fill = NA
    )

  }

  ## Median filter:
  if(method== "Median"){

    # apply to x positions:
    data$x <- zoo::rollapply(
      data$x,
      width = window_size,
      FUN = function(z) median(z, na.rm = TRUE),
      align = "center",
      fill = NA
    )

    # apply to y positions:
    data$y <- zoo::rollapply(
      data$y,
      width = window_size,
      FUN = function(z) median(z, na.rm = TRUE),
      align = "center",
      fill = NA
    )

  }

  ## Savitzky-Golay filter:
  if(method== "SG"){

    # remove NAs (if present):
    data<- na.omit(data)

    # apply filter to x and y positions:
    data$x<- signal::sgolayfilt(x = data$x, p = p, n = n)
    data$y<- signal::sgolayfilt(x = data$y, p = p, n = n)

  }


  return(data)


}
