library(shiny)
zonatorui <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
    )
  )
}

zonator <- function(id, data, parent_session){
  moduleServer(
    id,
    function(input,output, session){

    }
  )
}
