#' Launch Fresh Air Finder App
#' @export
freshAirFinder <- function() {
  appDir <- system.file("freshAirFinder", package = "freshAirFinderApp")
  shiny::runApp(appDir, display.mode = "normal")
}
