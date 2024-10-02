library(shiny)
library(shinycssloaders)
library(Seurat)
library(tidyverse)

geneexplorerui <- function(id){
  ns <- NS(id)
  bslib::page_sidebar(sidebar =
    bslib::sidebar(actionButton(ns("loadsinglecell"),
                                   label = "Load single cell data"),
                      shiny::selectizeInput(inputId = ns("genelist"),
                                            label = "Select gene from single cell dataset",
                                            choices = NULL),
                   bslib::tooltip(bsicons::bs_icon("info-circle",
                                                   title = "About Gene Explorer"),
                                  "Gene explorer allows the visualisation of gene
                                  expression for individual cells. An overview of
                                  cell types is included for convenience")),
    bslib::layout_columns(col_widths = c(6,6),
                          row_heights = c(2,2),
                          bslib::card(bslib::card_title("Gene Expression",
                                                        container = htmltools::h3),
                            bslib::card_body(plotOutput(ns("featureplot"))),
                            full_screen = T,
                            align = "center"
                                      ),
                          bslib::card(bslib::card_title("Reference plot for cell types",
                                                        container = htmltools::h3),
                                      bslib::card_body( plotOutput(ns("celltypeplot"))),
                                      align = "center"
                                      ),
                          bslib::card(bslib::card_title("Protein Abundance",
                                                        container = htmltools::h3),
                            bslib::card_body(uiOutput(outputId = ns("proteomicsviolin"))),
                            align = "center")
                            ))

}

geneexplorer <- function(id, data, parent_session){
  moduleServer(
    id,
    function(input,output, session){
      ns <- NS(id)
      req(data)
      geneselectormenu <- readRDS(here::here("data/geneselectormenu.rds"))


      observeEvent(input$loadsinglecell,{
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "Loading Data, this might take a while...", value  = 0)
        data$singlecell <- readRDS("data/slimmedsinglecelldata.rds")
        shiny::updateSelectizeInput(session,
                                    "genelist",
                                    choices = geneselectormenu,
                                    selected = "Tbc1d4",
                                    server = TRUE)
        progress$inc(1/1, detail = "Loading Done!")
      })

      ##Gene selector list is based on the following code:
      # Generates a list of genes in dropdown menu based on what genes are found
      #in the single cell dataset
      # singlecell <- readRDS("data/slimmedsinglecelldata.rds")
      # geneselectormenu <- rownames(singlecell)
      # saveRDS(geneselectormenu, here::here("data/geneselectormenu.rds"))


      #function checks if gene input in gene list changes. It then
      #plots a feature plot with the selected gene and checks if the gene is
      #also found in the proteomics list. If it is, it plots a violin plot. If
      #it is not, it prints that the gene cannot be found
      observeEvent(input$genelist,{
        output$featureplot <- renderPlot({
          req(data$singlecell)
          req(input$genelist!="")
          Seurat::FeaturePlot(data$singlecell,
                              features = input$genelist,
                              pt.size = 1)+
            ggplot2::ggtitle(input$genelist)+
            ggplot2::theme(title = ggplot2::element_text(size = 22),
                           axis.title = element_text(size = 12))
        })

        if (magrittr::is_in(input$genelist,data$protmatrix$Genes)==T){
          output$proteomicsviolin <- renderUI({
          data$violindata <- data$protmatrix |>
            dplyr::filter(Genes == input$genelist)
          plotOutput(ns("violinplot"))
          })
        }
        else{
          req(data$singlecell)
          output$proteomicsviolin <- renderUI({
              h4(paste(input$genelist,"  is not found in the proteomics dataset", sep = ""))
          })

        }
      })
      #should be changed to a selectizeInput down the line but I can't get it to work for now

      #code for the violinplot. This is only rendered if target gene is found in
      #the proteomics set
      output$violinplot <- renderPlot({
        req(data$violindata)

        ggplot2::ggplot(data$violindata, aes(x = Group,
                                             y = Abundance,
                                             fill = Group)
                        )+
          geom_violin()+
          stat_summary(fun = "median",
                       geom = "point",
                       size = 2,
                       color = "black")+
          ggplot2::theme_bw()+
          theme(axis.text.x = element_text(size = 16),
                axis.text.y = element_text(size = 12),
                axis.title.x = element_blank(),
                plot.title = element_text(size = 18,
                                          hjust = 0.5),
                legend.position = "none")+
          ggtitle(paste(input$genelist))+
          ggplot2::scale_fill_manual(values = c("#440154FF","#21908CFF","#443A83FF"))

      })

      #cell type plot. A plot to identify what cell type a gene is expressed in
      #when looking at the feature plot tab
      output$celltypeplot <- renderPlot({
        req(data$singlecell)
        Seurat::Idents(data$singlecell)<- "celltype"
        dimplot_colors <- viridis::viridis(n = 7)
        #some colors do not look too good. Change it
        dimplot_colors[6]<-"black"
        dimplot_colors[7]<-"#800000"
        Seurat::DimPlot(data$singlecell,
                                          pt.size = 0.7,
                                          label = T,
                                          label.size = 6,
                                          repel = T,
                                          cols = dimplot_colors,
                                          label.box = T,
                        label.color = "white",) +
          ggplot2::ggtitle("") +
          ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5,
                                               size = 24),
            legend.position = "none"
          )
      })
   }
  )
}
