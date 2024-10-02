library(shiny)
homeui <- function(id){
  ns <- NS(id)
  bslib::layout_columns(
    col_widths = c(3,6,3),
    column(12),
    column(12,
           h3("Welcome to Hepamorphosis", style = "text-align: center;"),

           bslib::card(bslib::card_title("Ready to get started? Select an option:"),
                       bslib::card_body(
                         shinyWidgets::actionBttn(inputId = ns("expressor"),
                                                  label = "Compare single cell expression to protein abundance",
                                                  style = "jelly"),
                         shinyWidgets::actionBttn(inputId = ns("correlator"),
                                                  label = "Correlate gene expression and protein abundance for your target gene",
                                                  style = "jelly"),
                         shinyWidgets::actionBttn(inputId = ns("zonator"),
                                                  label = "Map gene expression to central- or portal hepatocytes",
                                                  style = "jelly")
                       ),align = "center")),
    column(12)
  )

}

home <- function(id, data, parent_session){
  moduleServer(
    id,
    function(input,output, session){
      #react to button presses and go to relevant tab
      shiny::observeEvent(input$expressor,{
        shiny::updateTabsetPanel(session = parent_session,
                                 "inTabset",
                                 "explorer")
      })

      shiny::observeEvent(input$correlator,{
        shiny::updateTabsetPanel(session = parent_session,
                                 "inTabset",
                                 "correlator")
      })

      shiny::observeEvent(input$zonator,{
        shiny::updateTabsetPanel(session = parent_session,
                                 "inTabset",
                                 "zonator")
      })

    }
  )
}
