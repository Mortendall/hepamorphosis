library(shiny)
library(shinycssloaders)
library(Seurat)
library(tidyverse)

geneexplorerui <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(6,
             actionButton(ns("loadsinglecell"),
                          label = "Load single cell data"),
             shiny::selectizeInput(inputId = ns("genelist"),
                                   label = "Select gene from single cell dataset",
                                   choices = NULL)
    ),     column(6,
             plotOutput(ns("featureplot"))
             )),
    fluidRow(
      column(6,
             uiOutput(outputId = ns("proteomicsviolin"))
      ),     column(6,
                    plotOutput(ns("celltypeplot"))
      ))
  )
}

geneexplorer <- function(id, data, parent_session){
  moduleServer(
    id,
    function(input,output, session){
      ns <- NS(id)
      req(data)
      geneselectormenu <- readRDS(here::here("data/geneselectormenu.rds"))
      shiny::updateSelectizeInput(session,
                                  "genelist",
                                  choices = geneselectormenu,
                                  selected = "Tbc1d4",
                                  server = TRUE)

      observeEvent(input$loadsinglecell,{
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "Loading Data, this might take a while...", value  = 0)
        data$singlecell <- readRDS("data/updatedsinglecell.rds")
        progress$inc(1/1, detail = "Loading Done!")
      })

      ##Gene selector list is based on the following code:
      # Generates a list of genes in dropdown menu based on what genes are found
      #in the single cell dataset
      # singlecell <- readRDS("data/updatedsinglecell.rds")
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
          theme(axis.text.x = element_text(size = 16),
                axis.text.y = element_text(size = 12),
                axis.title.x = element_blank(),
                plot.title = element_text(size = 18,
                                          hjust = 0.5))+
          ggtitle(paste(input$genelist, " - Protein Abundance", sep = ""))

      })

      #cell type plot. A plot to identify what cell type a gene is expressed in
      #when looking at the feature plot tab
      output$celltypeplot <- renderPlot({
        req(data$singlecell)
        Seurat::Idents(data$singlecell)<- "celltype"
        Seurat::DimPlot(data$singlecell,
                                          pt.size = 0.7,
                                          label = T,
                                          label.size = 6,
                                          repel = T,
                                          cols = wesanderson::wes_palette(7,
                                                                          name = "FantasticFox1",
                                                                          type = "continuous"),
                                          label.box = F) +
          ggplot2::ggtitle("DimPlot By Cell Type") +
          ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5,
                                               size = 24),
            legend.position = "none"
          )
      })
   }
  )
}
