
library(shiny)


# Define UI
ui <- fluidPage(
  list(shiny::tags$head(shiny::HTML('<link rel="icon", href = "death-moth-clipart-md.png",
                                                  type = image/png" />'))),
  shiny::div(style = "padding: 1px 0px; width: '100%'",
             shiny::titlePanel(title = "Investigating primary mouse hepatocyte dedifferentiation",
                               windowTitle = "Enter Hepamorphosis")),

  theme = bslib::bs_theme(version = 5, bootswatch = "sandstone"),
  titlePanel("Hepamorphosis - Exploring Hepatocyte Dedifferentiation"),
  tabsetPanel(type = "tabs",
            id = "inTabset",
            tabPanel(title = "Home",
                     homeui("home")),
            #Home is a welcome page that presents the concept for the app
            tabPanel(title = "GeneExplorer",
                     geneexplorerui("geneexplorer"),
                     value = "explorer"),
            #Geneexplorer allows the investigation of specific genes/proteins
            tabPanel(title = "Correlator",
                     correlatorui("correlator"),
                     value = "correlator"),
            #correlator shows dotplot of logFC for RNAseq vs proteome
            tabPanel(title = "Zonator",
                     zonatorui("zonator"),
                     value = "zonator"),
            #Zonator allows user to investigate whether a specific gene is
            #zonated in liver
            tabPanel(title = "About Hepamorphosis",
                     aboutui("about"),
                     value = "about")
            )

)

# Define server logic. Built a modular server
server <- function(input, output, session) {
  #create the reactive object that will contain all the data for the app
  hepamorphosisdata <- reactiveValues()

  #load in proteomicsdata. It needs to be reformatted so it is displayed
  #correctly
  hepamorphosisdata$protmatrix<- readRDS("data/NormalizedMatrix.rds") |>
    as.data.frame() |>
    dplyr::select(!Protein.Ids) |>
    tidyr::pivot_longer(cols = !Genes,values_to = "Abundance",names_to = "ID") |>
    dplyr::mutate(Group = stringr::str_extract(ID, "^[:alpha:]+")) |>
    dplyr::mutate(Group = case_when(
      Group == "CS"~ "Cell Susp",
      Group == "PH"~"Prim Hep",
      .default = as.character(Group)
    )) |>
    dplyr::mutate(Group = factor(Group, levels = c("Liver", "Cell Susp", "Prim Hep"))) |>
    dplyr::filter(!Genes == "")

  hepamorphosisdata$rnamatrix<- readRDS("data/pseudohepanormalized.rds") |>
    as.data.frame() |>
    tidyr::pivot_longer(cols = -Gene,
                        values_to = "Expression",
                        names_to = "Sample") |>
    dplyr::mutate(Group = stringr::str_extract(Sample, "(?<=_)[:alpha:]+")) |>
    dplyr::mutate(Group = factor(Group, levels = c("Central", "Portal"))) |>
    dplyr::filter(!Gene == "")

  hepamorphosisdata$rnadeg <- readRDS("data/pseudoDEGhepasubset.rds")


  #load in differentially expressed genes and differentially abundant proteins
  #and create a list that contains all three comparisons

  hepamorphosisdata$correlation <- readRDS("data/correlationdata.rds")
  #rename to more pleasing names


  parent_session <- session
  home("home", hepamorphosisdata, parent_session)
  geneexplorer("geneexplorer", hepamorphosisdata, parent_session)
  correlator("correlator", hepamorphosisdata, parent_session)
  zonator("zonator", hepamorphosisdata, parent_session)
  about("about",hepamorphosisdata, parent_session)
}

# Run the application
shinyApp(ui = ui, server = server)
