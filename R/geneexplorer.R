library(shiny)
geneexplorerui <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(6,
             selectInput(inputId = ns("geneselector"),label = "test",choices = c("yes","no")),
             #renderUI())
    )),
    fluidRow(
      column(6,
             )
    )
  )
}

geneexplorer <- function(id, data, parent_session){
  moduleServer(
    id,
    function(input,output, session){

    }
  )
}
