
library(shiny)
hepamorphosisdata <- reactiveValues()
hepamorphosisdata$singlecell <- readRDS("data/220503_liver_full-seurat_updated.rds")
hepamorphosisdata$protmatrix<- readRDS("data/NormalizedMatrix.rds")
hepamorphosisdata$rnamatrix<- readRDS("data/pseudocounts.rds")
hepamorphosisdata$protdeg <- readRDS("data/DAPResults.rds")
hepamorphosisdata$rnadeg <- readRDS("data/pseudodeg.rds")

# Define UI
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "sandstone"),
  titlePanel("Hepamorphosis - Exploring Hepatocyte Dedifferentiation"),
  tabsetPanel(type = "tabs",
            id = "inTabset",
            tabPanel(title = "Home",
                     homeui("home")),
            #Home is a welcome page that presents the concept for the app
            tabPanel(title = "GeneExplorer",
                     geneexplorerui("genexplorer")),
            #Geneexplorer allows the investigation of specific genes/proteins
            tabPanel(title = "Correlator",
                     correlatorui("correlator")),
            #correlator shows dotplot of logFC for RNAseq vs proteome
            tabPanel(title = "Zonator",
                     zonatorui("zonator"))
            #Zonator allows user to investigate whether a specific gene is
            #zonated in liver
            )

)

# Define server logic
server <- function(input, output, session) {
  parent_session <- session
  home("home", hepamorphosisdata, parent_session)
  geneexplorer("geneexplorer", hepamorphosisdata, parent_session)
  correlator("correlator", hepamorphosisdata, parent_session)
  zonator("zonator", hepamorphosisdata, parent_session)
}

# Run the application
shinyApp(ui = ui, server = server)
