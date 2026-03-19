#' Launches a manual event labeler
#'
#' Opens a browser-based manual labeler for saccadic events.
#'
#' @return Invisibly returns the path to the HTML app.
#' @export
ManualEventLabeler <- function() {
  app <- system.file(
    "manual-labeler",
    "eye_tracking_manual_labeler.html",
    package = "eyemovements"
  )

  if (app == "") {
    stop(
      "Could not find the bundled manual labeler HTML file. ",
      "Please reinstall the package."
    )
  }

  utils::browseURL(app)
  invisible(app)
}
