aboutui <- function(id){
  ns <- NS(id)
  bslib::page_fluid(
    column(12,
           bslib::accordion(multiple = F,
                            bslib::accordion_panel(title = "What is hepamorphosis?",
                                                   "Hepamorphosis contains proteomics
                and single-nucleus RNAseq data from full liver, freshly
                isolated hepatocytes, and hepatocytes that has been cultured for 24h on
                collagen",
                icon = bsicons::bs_icon("question-circle")),
                bslib::accordion_panel(title = "Why is loading so slow?",
                                       icon = bsicons::bs_icon("hourglass-split"),
                                       "We know! Single cell data files are large,
                                    so it takes time to load in. Once data is loaded,
                                    the app should be easy to navigate"),
                bslib::accordion_panel(title = "Where can I find info on the groups and experimental conditions?",
                                       icon = bsicons::bs_icon("heart-pulse"),
                                       "Right here! Samples were acquired from Male C57BL/6NTac mice, ordered from Taconic.
                                    Samples were obtained from mice between 10 and 12 weeks of age. A catheter was inserted in the vena cava,
                                    and a liver sample (L) was obtained from the median lobe were snapfrozen.
                                    Following collagenase digestion, freshly isolated liver cells in solution (CS) were filtered, washed and snapfrozen.
                                    A subset were plated on collagen in MEM-199 w. 10% FBS and 1% P/S). After 2 hours of attachment, medium
                                    was changed to MEM-199 with 0.5% FBS, 1% P/S, 1 µM dexamethasone and 1 nM insulin. Cells were cultured for 24h
                                    at 37 °C with 5% CO2. On the following day, cells were washed in PBS, and harvested with trypsin. Trypsin was
                                    neutralized with culture medium (as above) and pellets were centrifuged, recovered and snap frozen. For more details, please
                                    see the reference."),
                bslib::accordion_panel(title = "What equipment/procedures were used to obtain the data?",
                                       icon = bsicons::bs_icon("eyedropper"),
                                       "Single nuclei were sorted using a SH800S Cell Sorter (SONY), 6,000 nuclei pr. sample
                                    when possible. Libraries were prepared and sequenced using an Illumina NovaSeq 6000. Proteomics
                                    analysis was performed on libraries using on 15 cm, 150 μM ID columns packed with C18 beads (1.5 μm; PepSep)
                                    on an Evosep ONE HPLC. Peptides were injected via a CaptiveSpray source and 10 μm emitter into a timsTOF pro2 mass spectrometer
                                    (Bruker) operated in DDA-PASEF or DIA-PASEF mode. A FragPipe-generated master spectral library was used in concert with DIA-PASEF
                                    mass spectra for analysis in DIA-NN (v1.8).  "),
                bslib::accordion_panel(title = "Where can I find the computational analysis?",
                                       "Code for the app is available here: https://github.com/Mortendall/hepamorphosis
                                    Code for the analysis and paper is available here: https://github.com/Mortendall/PrimHepMultiOmics ",
                                    icon = bsicons::bs_icon("file-text")),
                bslib::accordion_panel(title = "How do I cite this?",
                                       "[INSERT CITATION]",
                                       icon = bsicons::bs_icon("pencil")),
                bslib::accordion_panel(title = "How can I submit feedback?",
                                       tagList("Feedback can be sent to", a(href="mailto:dall@sund.ku.dk", "Morten Dall")
                                               ),
                                       icon = bsicons::bs_icon("envelope")),
                bslib::accordion_panel(title = "Version notes",
                                       icon = bsicons::bs_icon("clipboard-check"),
                                       shiny::HTML(
                                         "Version 0.0.1 (10/1-24) <br>
                                       - Optimized loading time <br>
                                       - Updated layout to bslib <br>
                                       - added about page"
                                       ))
           )
    )
  )

}

about <- function(id, data, parent_session){
  moduleServer(
    id,
    function(input,output, session){

    }
  )
}
