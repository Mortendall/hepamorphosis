library(shiny)
correlatorui <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
    )
  )
}

correlator <- function(id, data, parent_session){
  moduleServer(
    id,
    function(input,output, session){

    }
  )
}
