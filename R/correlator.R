library(shiny)
library(shinycssloaders)
correlatorui <- function(id,data){
  ns <- NS(id)
  bslib::page_sidebar(
    sidebar =
     bslib::sidebar(
       # shinycssloaders::withSpinner(
         uiOutput(ns("selectors"))
         # )
     ),
    bslib::layout_columns(height = 1300,
      bslib::card(full_screen = T,
                  uiOutput(ns("correlationplot"))
                  )
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
        names(data$correlation)<- c("Liver vs fresh cells",
                                                 "Liver vs cultured cells",
                                                 "Fresh cells vs cultured cells")
        div(
          selectizeInput(ns("correlatorselect"),
                      label = "Select a gene/protein pair to highlight",
                      choices = NULL),
          bslib::tooltip(bsicons::bs_icon("info-circle"),
                         "Missing your favorite target? The gene was likely not
                         found in the proteomics dataset, as mass spec technology
                         generally has lower depth than RNAseq. Alternatively,
                         see if your gene is known by an alias, and retry your search."),
          selectInput(ns("datasetselector"),
                      label = "Select a comparison",
                      choices = names(data$correlation),
                      selected = "Liver vs cultured cells"),
          bslib::tooltip(bsicons::bs_icon("info-circle"),
                         "All log-fold changes are indicated as \"left to right\".
                         So for liver vs. cultured cells, a logFC of 2 means
                         increased in liver, while -2 means decreased in liver.")
        )
      })

      #load selectizeInput on serversite
      shiny::observe({
        shiny::updateSelectizeInput(
          session,
          "correlatorselect",
          choices = data$correlation[[1]]$Genes,
          selected = "Nampt",
          server = TRUE)
      })


      #observe when a gene is selected from list
      observeEvent(input$correlatorselect,{
        output$correlationplot <- renderUI({
          plotly::plotlyOutput(ns("plotdata"),
                               height = 1200,
                               width = 1200)
        })
      })

      output$plotdata <- plotly::renderPlotly({
        req(input$correlatorselect)

        plot_color <- viridis::viridis(4)
        #avoid yellow in color palette
        plot_color[4]<- "black"

        p <- ggplot2::ggplot(data$correlation[[input$datasetselector]],
                        ggplot2::aes(x = protlogfc,
                                     y = rnalogfc,
                                     color = significant))+
          ggplot2::geom_point(size = 2
                               , ggplot2::aes(
             text = Genes)
            )+
          ggplot2::theme_bw()+
          ggplot2::xlab("Protein Log2FC")+
          ggplot2::ylab("Pseudobulk RNA  Log2FC")+
          ggplot2::ggtitle(input$datasetselector,
                           "Protein abundance vs. RNA expression")+
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,
                                                            size = 24),
                         plot.subtitle = ggplot2::element_text(hjust = 0.5,
                                                               size = 22),
                         axis.title = ggplot2::element_text(size = 20),
                         axis.text = ggplot2::element_text(size = 20))+
          ggplot2::scale_color_manual(values = plot_color)

       q <- plotly::ggplotly(p,tooltip = "text") |> plotly::add_annotations(x = subset(data$correlation[[input$datasetselector]], Genes==input$correlatorselect)$protlogfc,
                                                       y = subset(data$correlation[[input$datasetselector]], Genes==input$correlatorselect)$rnalogfc,
                                                       text = input$correlatorselect,
                                                       bgcolor = "white",
                                                       bordercolor = "black")
      })
    }
  )
}
