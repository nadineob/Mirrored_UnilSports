#' @export
UnilSports_gui <- function(){
  appDir = system.file("UnilSports_int", package = "UnilSports")
  shiny::runApp(appDir, display.mode = "normal")
}