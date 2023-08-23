library(shiny)
zonatorui <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(4,
             actionButton(ns("loadhepatocytes"),
                          label = "Load hepatocyte data"),
             shiny::selectizeInput(inputId = ns("zonegenelist"),
                                   label = "Select gene from single cell dataset",
                                   choices = NULL)
             ,
             uiOutput(ns("RNAviolin")),
             tableOutput(ns("stats"))),
      column(8,
             plotOutput(ns("zonefeatureplot")),
             plotOutput(ns("zoneidentifier"))
      )
    )
  )
}

zonator <- function(id, data, parent_session){
  moduleServer(
    id,
    function(input,output, session){
      ns <- NS(id)
      hepatocytechoices <- readRDS(here::here("data/hepatocyteselector.rds"))
      shiny::updateSelectizeInput(session,
                                  "zonegenelist",
                                  choices = hepatocytechoices,
                                  selected = "Tbc1d4",
                                  server = TRUE)
      #check when loadhepatocytes is clicked
      observeEvent(input$loadhepatocytes,{
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "Loading Data, this might take a while...", value  = 0)
        data$hepatocytesinglecell <- readRDS("data/hepatocyte_subset.rds")
        progress$inc(1/1, detail = "Loading Done!")
      })

      #define the gene selector
      #test <- readRDS(here::here("data/hepatocyte_subset.rds"))
      #hepatocytechoices <- rownames(test)
      #saveRDS(hepatocytechoices, here::here("data/hepatocyteselector.rds"))

      #Define code for violinplot and featureplot
      observeEvent(input$zonegenelist,{
        output$zonefeatureplot <- renderPlot({
          req(data$hepatocytesinglecell)
          req(input$zonegenelist!="")
          Seurat::FeaturePlot(data$hepatocytesinglecell,
                              features = input$zonegenelist,
                              pt.size = 1)+
            ggplot2::ggtitle(input$zonegenelist)+
            ggplot2::theme(title = ggplot2::element_text(size = 22),
                           axis.title = element_text(size = 12))
        })

          output$RNAviolin <- renderUI({
            data$zoneviolindata <- data$rnamatrix |>
              dplyr::filter(Gene == input$zonegenelist)
            plotOutput(ns("zoneviolinplot"))
          })
      })

      #define code for violinplot
      output$zoneviolinplot <- renderPlot({
        req(data$zoneviolindata)
        ggplot2::ggplot(data$zoneviolindata, aes(x = Group,
                                             y = Expression,
                                             fill = Group)
        )+
          geom_violin()+
          stat_summary(fun = "median",
                       geom = "point",
                       size = 2,
                       color = "black")+
          theme(axis.text.x = element_text(size = 16),
                axis.text.y = element_text(size = 12),
                axis.title.x = element_blank(),
                plot.title = element_text(size = 18,
                                          hjust = 0.5))+
          ggtitle(paste(input$zonegenelist, " - Pseudocount CPM", sep = ""))

      })
      #Code for annotated cell type plot for hepatocyte subset

      output$zoneidentifier <- renderPlot({
        req(data$hepatocytesinglecell)
        Seurat::Idents(data$hepatocytesinglecell)<- "Zone"
        Seurat::DimPlot(data$hepatocytesinglecell,
                        pt.size = 0.7,
                        label = T,
                        label.size = 6,
                        repel = T,
                        cols = wesanderson::wes_palette(7,
                                                        name = "FantasticFox1",
                                                        type = "continuous"),
                        label.box = F) +
          ggplot2::ggtitle("DimPlot By Zone") +
          ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5,
                                               size = 24),
            legend.position = "none"
          )
      })

      #define table to be displayed with DEGs
      output$stats <- renderTable({
        req(data$hepatocytesinglecell)
        data$hepatocytedeg <- data$rnadeg |>
          dplyr::filter(gene == input$zonegenelist) |>
          dplyr::select(gene, log2FoldChange, padj)
        data$hepatocytedeg

      })


    }
  )
}
