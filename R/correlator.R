library(shiny)
library(shinycssloaders)
correlatorui <- function(id,data){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(3,
             shinycssloaders::withSpinner(uiOutput(ns("selectors")))),
      column(9,
             uiOutput(ns("correlationplot")))
    )
  )
}

correlator <- function(id, data, parent_session){
  moduleServer(
    id,
    function(input,output, session){
      ns <- NS(id)

      output$selectors <- renderUI({
        req(data$correlation)
        div(
          selectInput(ns("correlatorselect"),
                      label = "Select a gene/protein pair to highlight",
                      choices = data$correlation[[1]]$Genes,
                      selected = "Nampt"),
          selectInput(ns("datasetselector"),
                      label = "Select a comparison",
                      choices = names(data$correlation),
                      selected = "L_vs_PH")
        )
      })


      #observe when a gene is selected from list
      observeEvent(input$correlatorselect,{
        output$correlationplot <- renderUI({
          plotly::plotlyOutput(ns("plotdata"))
        })
      })

      output$plotdata <- plotly::renderPlotly({
        req(input$correlatorselect)

        p <- ggplot2::ggplot(data$correlation[[input$datasetselector]],
                        ggplot2::aes(x = protlogfc,
                                     y = rnalogfc,
                                     color = significant))+
          ggplot2::geom_point(size = 2, aes(
            text = Genes
            ))+
          ggplot2::theme_bw()+
          ggplot2::xlab("Protein Ab. Log2FC")+
          ggplot2::ylab("Pseudobulk RNA exp. Log2FC")+
          ggplot2::ggtitle(input$datasetselector,
                           "Protein abundance vs. RNA expression")+
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,
                                                            size = 24),
                         plot.subtitle = ggplot2::element_text(hjust = 0.5,
                                                               size = 22),
                         axis.title = ggplot2::element_text(size = 20),
                         axis.text = ggplot2::element_text(size = 20))+
          ggplot2::scale_color_manual(values = wesanderson::wes_palette("Moonrise2",4))

       q <- plotly::ggplotly(p,tooltip = "text") |> plotly::add_annotations(x = subset(data$correlation[[input$datasetselector]], Genes==input$correlatorselect)$protlogfc,
                                                       y = subset(data$correlation[[input$datasetselector]], Genes==input$correlatorselect)$rnalogfc,
                                                       text = input$correlatorselect,
                                                       bgcolor = "white",
                                                       bordercolor = "black")
      })
    }
  )
}
