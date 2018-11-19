#' Run the Shiny Set Up application
#' 
#' Use this Shiny app to set up the OM and EM for use with WKFORBIAS package.
#' @export

runShiny <- function(){
  appDir <- system.file("shiny-examples", "SetUp", package = "WKFORBIAS")
  if (appDir == ""){
    stop("Could not find Shiny example directory. Try re-installing 'WKFORBIAS'.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}
