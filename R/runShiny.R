#' Run the Shiny Set Up application
#' 
#' @export

runShiny <- function(){
  appDir <- system.file("shiny-examples", "SetUp", package = "WKFORBIAS")
  if (appDir == ""){
    stop("Could not find Shiny example directory. Try re-installing 'WKFORBIAS'.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}
