library(shiny)
homeui <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(12,
             h3("Welcome to Hepamorphosis!"),
              h4("Hepamorphosis contains proteomics
                and single-nucleus RNAseq data from full liver, freshly
                isolated hepatocytes that has been cultured for 24h on
                collagen"),
             h4("As the datasets are quite large, it takes some time for the
                tabs to load. Sorry about the inconvenience!"),
             h6("Written by Morten Dall, Staff Scientist, from the Novo
                Nordisk Foundation Center for Basic Metabolic Research,
                University of Copenhagen"),
             h6("For comments or questions, please contact dall@sund.ku.dk"))
    )
  )
}

home <- function(id, data, parent_session){
  moduleServer(
    id,
    function(input,output, session){

    }
  )
}
