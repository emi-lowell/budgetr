#' details UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_details_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' details Server Functions
#'
#' @noRd 
mod_details_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_details_ui("details_1")
    
## To be copied in the server
# mod_details_server("details_1")
