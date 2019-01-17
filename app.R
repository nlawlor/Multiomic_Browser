  library(shiny)
library(Sushi)
library(ggplot2)
library(DT)
library(shinyjs)
library(shinythemes)
library(GenomicRanges)
library(IRanges)
library(InteractionSet)
library(rintrojs)

# Define UI
ui <- shinyUI(fluidPage(theme = shinytheme("cerulean"),

                  # google analytics tracking      
                  tags$head(includeScript("google-analytics.js")),

                  useShinyjs(),
                  includeCSS("www/animate.min.css"),
                  
                  # loading page animation
                  div(id = "loading-content",
                      class = "loading-content",
                      h1("Welcome to the Shiny App for Visualizing EndoC-βH1 and Human Islet Genomics Data!", align = "center"),
                      h1(class = "animated infinite pulse", "Loading data, please wait...",
                         align = "center")
                  ),
                  # hide main panel until load data
                  hidden(
                    div(
                      id = "main_content",
                      
                      # Application title
                      titlePanel(div("Welcome to the Shiny App for Visualizing EndoC-βH1 and Human Islet Genomics Data!", 
                                     img(height = 88, width = 217, 
                                         src = "jax.logo.gif", 
                                         class = "pull-right"))),
                      # side bar where user first interacts 
                      sidebarPanel(
                        # call function to use package rintrojs                                
                        introjsUI(),
                        # add first button for guided tutorial
                        introBox(
                          actionButton("help", "Click here for a guided tutorial of the app!", icon = icon("list-ol")),
                          data.step = 1,
                          data.intro = "This is the start of the tutorial. Click 'Next Step' to continue."
                        ),
                        
                        # Or provide download button for manual
                        introBox(
                          downloadButton("Download_Manual", "Download Tutorial Document"),
                          data.step = 2,
                          data.intro = "Click this button to download an in-depth tutorial/manual for this app."
                        ),
                        introBox(
                          # select data types to plot
                          h4("1a. Select Data Types to Visualize"),
                          checkboxGroupInput(inputId = "EndoCData", label = "EndoC-βH1 Data", 
                                             choices = c("Hi-C", "ChIA-PET", "Chromatin-States",
                                                         "ATAC-seq", "RNA-seq", "Genes"),
                                             inline = TRUE, selected = c("Hi-C", "ChIA-PET", "Chromatin-States",
                                                                         "ATAC-seq", "RNA-seq", "Genes")),
                          checkboxGroupInput(inputId = "IsletData", label = "Human Islet Data", 
                                             choices = c("Hi-C", "Chromatin-States",
                                                         "ATAC-seq", "RNA-seq"),
                                             inline = TRUE, selected = c("Hi-C", "Chromatin-States",
                                                                         "ATAC-seq", "RNA-seq")),
                          data.step = 3, data.intro = "Click the check boxes here to indicate which data types you want to visualize.",
                          data.hint = "You can mark/unmark these check boxes."),
                        introBox(
                          # specify color schemes for plots
                          h4("1b. Choose color schemes"),
                          div(style="display: inline-block;vertical-align:top; width: 200px;",
                              selectInput(inputId = "endoc_color", label = "EndoC-βH1 Color",
                                          choices = c("red", "blue", "green", "purple", "orange", "yellow", "black", "brown", "pink", "grey"), selected = "red")),
                          div(style="display: inline-block;vertical-align:top; width: 200px;",
                              selectInput(inputId = "islet_color", label = "Islet Color",
                                          choices = c("red", "blue", "green", "purple", "orange", "yellow", "black", "brown", "pink", "grey"), selected = "black")),
                          data.step = 4, data.intro = "Click these drop-down menus to specify the colors of the EndoC/Islet tracks to be plotted."),
                        # type in gene symbol to update input coordinates to gene location
                        introBox(
                          h4("2a. Enter Gene Symbol (to get genome location)"),
                          div(style="width: 200px;",
                              textInput(inputId = "gene", label = "Gene symbol (e.g., ISL1): ", value = "ISL1",  placeholder = "")),
                          shiny::actionButton("do_gene", "Get Gene Coordinates", icon = icon("search")),
                          data.step = 5, data.intro = "Enter in a human gene symbol and click the 'Get Gene Coordinates' button. The chromosome number, start position, and end position will be updated in Panel 2b below.", 
                          data.hint = "Gene symbols should contain capital letters. If the gene is not valid or in the database, no chromosome number nor positions will be updated."), 
                        # manual input of coordinates
                        introBox(
                          h4("2b. Or Enter Genome Coordinates (hg19) manually"),
                          div(style="width: 300px;",
                              textInput(inputId = "chrom", label = "Chromosome number (chr1-chr22): ", value = "chr9",  placeholder = "")),
                          div(style="width: 300px;",
                              numericInput(inputId = "chromstart", label = "Chromosome start (no commas): ", value = 2475722)),
                          div(style="width: 300px;",
                              numericInput(inputId = "chromend", label = "Chromosome end (no commas): ", value = 5648441)),
                          data.step = 6, data.intro = "Alternatively to Step 2a, you may provide the chromosome number (autosomes only) and positions manually.",
                          data.hint = "Please note the chromosome start and end numbers should not contain commas."),
                        # FINAL button to submit coordinates to make plot
                        introBox(
                          h3("3. View Data"),
                          shiny::actionButton("do", "Submit Coordinates", icon = icon("paper-plane"), class = "btn-primary"),
                          #style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                          data.step = 7, data.intro = "Clicking this button will generate the multiomics plot. Generating the plot may take a few seconds and will be displayed to the right of this sidebar panel in the 'Multiomic Plot' tab.",
                          data.hint = "To view an updated multiomics plot after changing the genome coordinates (either manually, or via gene symbol search), don't forget to click this 'Submit Coordinates' button!"),
                        br(),
                        h4("Author: "),
                        introBox(
                          tags$div(class = "header", checked = NA,  
                                   tags$i("Nathan Lawlor (nathan.lawlor03@gmail.com)"),
                                   tags$p(""),
                                   tags$a(href = "https://www.jax.org/research-and-faculty/research-labs/the-stitzel-lab",
                                          "Visit the Stitzel Lab Here!", target = "_blank")),
                          data.step = 12, data.intro = "Please contact me with any questions about the app. I'm happy to help! Also, checkout the Stitzel Lab website :)"),
                        h4("Acknowledgements: "),
                        tags$div(class = "header", checked = NA,  
                                 tags$i("This app is a wrapper for the R package Sushi:"),
                                 tags$p(""),
                                 tags$a(href = "https://bioconductor.org/packages/release/bioc/html/Sushi.html",
                                        "Check out Sushi Here!", target = "_blank")),
                        h4("Questions?"),
                        tags$div(class = "header", checked = NA,  
                                 tags$i(""),
                                 tags$p(""),
                                 tags$a(href = "https://github.com/nlawlor/Multiomic_Browser",
                                        "Visit the App Github page here!", target = "_blank"))
                        
                      ),
                      
                      # main panel with multiple tabs
                      introBox(
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Multiomic Plot", downloadButton("Download_Plot", "Download Plot"), 
                                     # choose to highlight specific coordinates
                                     wellPanel(
                                       h4("Highlight Genome Coordinates on Plot (Optional)"),
                                       introBox(
                                         div(style="display: inline-block;vertical-align:top; width: 200px;", selectInput(inputId = "highlight", label = "Highlight Regions?", choices = c("Yes", "No"),
                                                                                                                          selected = "Yes")),
                                         data.step = 8, data.intro = "You can indicate here whether or not you'd like to highlight specific genomic regions on the resultant plot by clicking this drop down menu and choosing Yes or No."),
                                       introBox(
                                         div(style="display: inline-block;vertical-align:top; width: 150px;", numericInput(inputId = "pair1_start", label = "Chromosome start1 (no commas): ", value = 2860000)),
                                         div(style="display: inline-block;vertical-align:top; width: 150px;", numericInput(inputId = "pair1_end", label = "Chromosome end1 (no commas): ", value = 2870000)),
                                         div(style="display: inline-block;vertical-align:top; width: 150px;", numericInput(inputId = "pair2_start", label = "Chromosome start2 (no commas): ", value = 3180000)),
                                         div(style="display: inline-block;vertical-align:top; width: 150px;", numericInput(inputId = "pair2_end", label = "Chromosome end2 (no commas): ", value = 3190000)),
                                         data.step = 9, data.intro = "If you chose 'Yes' to highlight regions, you can specify up to two genomic regions to highlight. Please provide the start and end positions of each pair of regions here. If you selected 'No' you can skip this step.")
                                     ),
                                     # zoom in/out buttons
                                     wellPanel(
                                       h4("Zoom In/Out"),
                                       introBox(
                                         shiny::actionButton("zoom_in_1.5", "Zoom In 1.5X", icon = icon("search-plus")),
                                         shiny::actionButton("zoom_in_3", "Zoom In 3X", icon = icon("search-plus")),
                                         shiny::actionButton("zoom_out_1.5", "Zoom Out 1.5X", icon = icon("search-minus")),
                                         shiny::actionButton("zoom_out_3", "Zoom Out 3X", icon = icon("search-minus")),
                                         data.step = 10, data.intro = "Click any of these zoom buttons to change the view of your resulting plot.")),
                                     h4("Plot"),
                                     wellPanel(
                                       plotOutput("endocPlot", height = 1600, width = 1600, brush = brushOpts(id = "plot_brush", clip = FALSE)))),
                            tabPanel("Selected Coordinates", verbatimTextOutput("select")),
                            tabPanel("Overlapping GWAS SNPs", downloadButton("Download_SNPs", "Download GWAS SNPs"), DT::dataTableOutput("snptable")),
                            tabPanel("Nearby Chromatin Interactions", downloadButton("Download_Nearby", "Download Nearby Interactions"), DT::dataTableOutput("chromatin_ints")),
                            tabPanel("EndoC Hi-C", downloadButton("Download_EndoC_HIC", "Download EndoC Hi-C"), DT::dataTableOutput("endohic")),
                            tabPanel("Islet Hi-C", downloadButton("Download_Islet_HIC", "Download Islet Hi-C"), DT::dataTableOutput("islethic")),
                            tabPanel("EndoC ChIA-PET", downloadButton("Download_EndoC_ChIA", "Download EndoC ChIAPET"), DT::dataTableOutput("chia")),
                            tabPanel("EndoC Chromatin States", downloadButton("Download_EndoC_Chrom", "Download EndoC Chromatin States"), DT::dataTableOutput("info")),
                            tabPanel("Islet Chromatin States", downloadButton("Download_Islet_Chrom", "Download Islet Chromatin States"), DT::dataTableOutput("islet")),
                            tabPanel("EndoC ATAC-seq", downloadButton("Download_EndoC_ATAC", "Download EndoC ATAC-seq"), DT::dataTableOutput("endocatac")),
                            tabPanel("Islet ATAC-seq", downloadButton("Download_Islet_ATAC", "Download Islet ATAC-seq"), DT::dataTableOutput("isletatac")),
                            tabPanel("Genes", downloadButton("Download_Genes", "Download Genes"), DT::dataTableOutput("gens"))
                          )
                        ),
                        data.step = 11, data.intro = "This is the main tab of the application. Here you can view plotted data and access specific tables.
                        The 1st tab 'Multiomic Plot' will display the image. To use the 2nd tab, first, click the 'Multiomic Plot' tab, and use your mouse to click and drag over the image to highlight a specific region.
                        Then, if you click this 'Selected Coordinates' tab again, it should indicate the chromosome, start, and end position that you have selected.
                        The 3rd tab contains a table of Genome wide association study (GWAS) single nucleotide polymorphisms (SNPs) that occur within the regions displayed in your plot.
                        The 4th tab contains a table of all chromatin interactions (ChIA-PET, Hi-C) that intersect the regions displayed in your plot. Note, this table is helpful for identifying interactions that extend beyond your plotting window.
                        Tabs 5-7 contain tables of chromatin interactions in your plot window or in the region you selected with your mouse (if you chose to do so). Nearest gene annotations to each node are provided.
                        Tabs 8-9 contain tables of chromatin state information,
                        Tabs 10-11 contain tables of ATAC-seq peaks and corresponding annotations, and 
                        lastly, tab 12 contains a table of genes.")
  ))
))







# Define server logic required to plot data
server <- shinyServer(function(input, output, session) {
  
  ########################################################
  # load endoC data
  ########################################################
  chia <- withProgress(expr = readRDS("Data/EndoC.compressed.omics.ChIA.PET.Rds"),
                       message = "Loading EndoC-βH1 ChIA-PET data, please wait")
  atac <- withProgress(expr = readRDS("Data/EndoC.compressed.omics.ATAC.Rds"),
                       message = "Loading EndoC-βH1 ATAC-seq data, please wait")
  rna <- withProgress(expr = readRDS("Data/EndoC.compressed.omics.RNA.Rds"),
                      message = "Loading EndoC-βH1 RNA-seq data, please wait")
  chromhmm_ext <- withProgress(expr = readRDS("Data/EndoC.compressed.omics.ChromHMM.Rds"),
                               message = "Loading EndoC-βH1 Chromatin States data, please wait")
  snps.df <- withProgress(expr = readRDS("Data/EndoC.compressed.omics.SNPs.Rds"),
                          message = "Loading GWAS SNPs, please wait")
  gtf.res <- withProgress(expr = readRDS("Data/EndoC.compressed.omics.genes.extend.Rds"),
                          message = "Loading genes, please wait")
  hic.sel <- withProgress(expr = readRDS("Data/EndoC.compressed.omics.HiC.Rds"),
                          message = "Loading EndoC-βH1 Hi-C data, please wait")
  peaks <- withProgress(expr = readRDS("Data/EndoC.compressed.omics.ATAC.peak.regions.Rds"),
                        message = "Loading EndoC-βH1 ATAC-seq Peaks, please wait")
  
  # load islet data
  islet_hic_sel <- withProgress(expr = readRDS("Data/Islet.HiC.compressed.Rds"),
                                message = "Loading Islet Hi-C data, please wait")
  islet_atac <- withProgress(expr = readRDS("Data/Islet.ATAC.compressed.Rds"),
                             message = "Loading Islet ATAC-seq data, please wait")
  islet_rna <- withProgress(expr = readRDS("Data/Islet.RNA.compressed.Rds"),
                            message = "Loading Islet RNA-seq data, please wait")
  islet_chromhmm_ext <- withProgress(expr = readRDS("Data/Islet.chromHMM.compressed.Rds"),
                                     message = "Loading Islet Chromatin states data, please wait")
  islet_peaks <- withProgress(expr = readRDS("Data/Islet.compressed.omics.ATAC.peak.regions.Rds"),
                              message = "Loading Islet ATAC-seq Peaks, please wait")
  
  # after data finishes loading, change from loading page to main page
  hide("loading-content")
  shinyjs::show("main_content")
  
  ###########################################################################################
  # Functions to plot data
  ###########################################################################################
  # wrapper to sushi plotting functions
  generate_plot1 <- function(chrom, chromstart, chromend, pair1_start, pair1_end, pair2_start, pair2_end,
                             highlight_regions = TRUE, choice_endoc = c("Hi-C", "ChIA-PET", "Chromatin-States",
                                                                        "ATAC-seq", "RNA-seq", "Genes"),
                             choice_islet = c("Hi-C", "Chromatin-States",
                                              "ATAC-seq", "RNA-seq")) {
    # zoom regions
    zoomregion1 <- c(pair1_start, pair1_end)
    zoomregion2 <- c(pair2_start, pair2_end)
    # plot endoC hic
    if (any(choice_endoc %in% "Hi-C")) {
      p_endoc_hic <- plotBedpe(hic.sel, chrom, chromstart, chromend,
                               color = input$endoc_color, flip = TRUE,
                               plottype="lines", lwd = 1, bty = "n")
      labelplot(letter = "", title = "EndoC Hi-C", titlecol = input$endoc_color)
      labelgenome(chrom,chromstart,chromend,n=4,scale="Mb", scalecex = 1.0, cex.axis = 1.5)
      if (highlight_regions) {
        zoomsregion(zoomregion1, highlight = TRUE)
        zoomsregion(zoomregion2, highlight = TRUE)
      }
    } else {}
    
    # plot islet hic
    if (any(choice_islet %in% "Hi-C")) {
      p_islet_hic <- plotBedpe(islet_hic_sel, chrom, chromstart, chromend,
                               color = input$islet_color, flip = TRUE,
                               plottype="lines", lwd = 1, bty = "n")
      labelplot(letter = "", title = "Islet Hi-C", titlecol = input$islet_color)
      labelgenome(chrom,chromstart,chromend,n=4,scale="Mb", scalecex = 1.0, cex.axis = 1.5)
      if (highlight_regions) {
        zoomsregion(zoomregion1, highlight = TRUE)
        zoomsregion(zoomregion2, highlight = TRUE)
      }
    } else {}
    
    # plot endoC chia-pet
    if (any(choice_endoc %in% "ChIA-PET")) {
      p_endoc_chia <- plotBedpe(chia,chrom,chromstart,chromend,
                                color = input$endoc_color,
                                heights = chia$score,plottype="loops")
      labelplot(letter = "", title = "EndoC ChIA-PET (Pol2)", titlecol = input$endoc_color)
      mtext("Score",side=2,line=2,cex=1,font=2)
      axis(side=2,las=2,tcl=.2)
      labelgenome(chrom,chromstart,chromend,n=4,scale="Mb", scalecex = 1.0, cex.axis = 1.5)
      if (highlight_regions) {
        zoomsregion(zoomregion1, highlight = TRUE)
        zoomsregion(zoomregion2, highlight = TRUE)
      }
    } else {}
    
    # if plot any chromatin states (include legend first)
    if (any(choice_endoc %in% "Chromatin-States") | any(choice_islet %in% " Chromatin-States")) {
      # chromatin state legends
      pchrom <- plotBed(chromhmm_ext[,1:3], chrom = chrom, chromstart = chromstart, chromend = chromend,
                        type = "region", color = "white", row = "supplied", height = 0.01)
      legend("topleft",inset =0.01,legend=c("Weak transcription", "Strong transcription",
                                         "Repressed polycomb", "Weak repressed polycomb",
                                         "Genic enhancer"),
             col=levels(chromhmm_ext$Color)[1:5], pch=19, text.font=2, pt.cex = 2,
             bg = "grey95", cex = 2, horiz = TRUE)
      legend("bottomleft",inset =0.01,legend=c("Poised TSS", "Active TSS",
                                         "Weak TSS", "Active enhancer",
                                         "Weak enhancer", "Low signal/Quiescent"),
             col=levels(chromhmm_ext$Color)[6:11], pch=19, text.font=2, pt.cex = 2,
             bg = "grey95", cex = 2,  horiz = TRUE)
      labelplot(letter = "", title = "Chromatin States (chromHMM) Legend", titlecol = "black")
    }
    
    # plot endoC chromatin states
    if (any(choice_endoc %in% "Chromatin-States")) {
      # error message if there are no bed coordinates to plot
      id_ec <- which(chromhmm_ext$Chr == chrom & chromhmm_ext$Start >= chromstart & chromhmm_ext$End <= chromend)
      if (length(id_ec) == 0) {
        # determine which element is intersecting
        elm_gr <- GRanges(seqnames = chromhmm_ext$Chr, IRanges(start = chromhmm_ext$Start, end = chromhmm_ext$End))
        inp_gr <- GRanges(seqnames = chrom, IRanges(start = chromstart, end = chromend))
        # find element intersecting
        elm_int <- findOverlaps(query = inp_gr, subject = elm_gr)
        sub_ov <- chromhmm_ext[subjectHits(elm_int),]
        # change element start and end to window
        sub_ov$Start <- as.numeric(chromstart) + 1
        sub_ov$End <- as.numeric(chromend) - 1 
        # plot as usual
        p_endoc_chrom <- plotBed(sub_ov[,1:3], chrom = chrom, chromstart = chromstart, chromend = chromend,
                                 type = "region", color = as.character(sub_ov$Color), row = "supplied", height = 0.01, plotbg="grey95")
        labelgenome(chrom, chromstart, chromend, n=4,scale="Mb",edgeblankfraction=0.10, scalecex = 1.0, cex.axis = 1.5)
        labelplot(letter = "", title = "EndoC Chromatin States", titlecol = input$endoc_color)
      }
      else {
        p_endoc_chrom <- plotBed(chromhmm_ext[,1:3], chrom = chrom, chromstart = chromstart, chromend = chromend,
                                 type = "region", color = as.character(chromhmm_ext$Color), row = "supplied", height = 0.01, plotbg="grey95")
        labelgenome(chrom, chromstart, chromend, n=4,scale="Mb",edgeblankfraction=0.10, scalecex = 1.0, cex.axis = 1.5)
        labelplot(letter = "", title = "EndoC Chromatin States", titlecol = input$endoc_color)
      }
    }
    
    # plot islet chromatin states
    if (any(choice_islet %in% "Chromatin-States")) {
      id_ic <- which(islet_chromhmm_ext$Chr == chrom & islet_chromhmm_ext$Start >= chromstart & islet_chromhmm_ext$End <= chromend)
      if (length(id_ic) == 0) {
        # determine which element is intersecting
        elm_gr <- GRanges(seqnames = islet_chromhmm_ext$Chr, IRanges(start = islet_chromhmm_ext$Start, end = islet_chromhmm_ext$End))
        inp_gr <- GRanges(seqnames = chrom, IRanges(start = chromstart, end = chromend))
        # find element intersecting
        elm_int <- findOverlaps(query = inp_gr, subject = elm_gr)
        sub_ov <- islet_chromhmm_ext[subjectHits(elm_int),]
        # change element start and end to window
        sub_ov$Start <- as.numeric(chromstart) + 1
        sub_ov$End <- as.numeric(chromend) - 1
        # plot as usual
        p_endoc_chrom <- plotBed(sub_ov[,1:3], chrom = chrom, chromstart = chromstart, chromend = chromend,
                                 type = "region", color = as.character(sub_ov$Color), row = "supplied", height = 0.01, plotbg="grey95")
        labelgenome(chrom, chromstart, chromend, n=4,scale="Mb",edgeblankfraction=0.10, scalecex = 1.0, cex.axis = 1.5)
        labelplot(letter = "", title = "Islet Chromatin States", titlecol = input$islet_color)
      }
      else {
        p_islet_chrom <- plotBed(islet_chromhmm_ext[,1:3], chrom = chrom, chromstart = chromstart, chromend = chromend,
                                 type = "region", color = as.character(islet_chromhmm_ext$Color), row = "supplied", height = 0.01, plotbg="grey95")
        labelgenome(chrom, chromstart, chromend, n=4,scale="Mb",edgeblankfraction=0.10, scalecex = 1.0, cex.axis = 1.5)
        labelplot(letter = "", title = "Islet Chromatin States", titlecol = input$islet_color)
      }
    }
    
    # plot endoC ATAC-seq
    if (any(choice_endoc %in% "ATAC-seq")) {
      p_endoc_atac <- plotBedgraph(atac,chrom,chromstart,chromend, color = input$endoc_color)
      labelgenome(chrom,chromstart,chromend,n=4,scale="Mb", scalecex = 1.0, cex.axis = 1.5)
      mtext("Read Depth",side=2,line=2,cex=0.75,font=2)
      axis(side=2,las=2,tcl=.2)
      labelplot(letter = "", title = "EndoC ATAC-seq", titlecol = input$endoc_color)
      if (highlight_regions) {
        zoomsregion(zoomregion1, highlight = TRUE)
        zoomsregion(zoomregion2, highlight = TRUE)
      }
    } else {}
    
    # plot islet ATAC-seq
    if (any(choice_islet %in% "ATAC-seq")) {
      p_islet_atac <- plotBedgraph(islet_atac,chrom,chromstart,chromend, color = input$islet_color)
      labelgenome(chrom,chromstart,chromend,n=4,scale="Mb", scalecex = 1.0, cex.axis = 1.5)
      mtext("Read Depth",side=2,line=2,cex=0.75,font=2)
      axis(side=2,las=2,tcl=.2)
      labelplot(letter = "", title = "Islet ATAC-seq", titlecol = input$islet_color)
      if (highlight_regions) {
        zoomsregion(zoomregion1, highlight = TRUE)
        zoomsregion(zoomregion2, highlight = TRUE)
      }
    } else {}
    
    # plot endoc rna-seq
    if (any(choice_endoc %in% "RNA-seq")) {
      p_endoc_rna <- plotBedgraph(rna,chrom,chromstart,chromend, color = input$endoc_color)
      labelgenome(chrom,chromstart,chromend,n=4,scale="Mb", scalecex = 1.0, cex.axis = 1.5)
      mtext("Read Depth",side=2,line=2,cex=0.75,font=2)
      axis(side=2,las=2,tcl=.2)
      labelplot(letter = "", title = "EndoC RNA-seq", titlecol = input$endoc_color)
      if (highlight_regions) {
        zoomsregion(zoomregion1, highlight = TRUE)
        zoomsregion(zoomregion2, highlight = TRUE)
      }
    } else {}
    
    # plot islet rna-seq
    if (any(choice_islet %in% "RNA-seq")) {
      p_islet_rna <- plotBedgraph(islet_rna,chrom,chromstart,chromend, color = input$islet_color)
      labelgenome(chrom,chromstart,chromend,n=4,scale="Mb", scalecex = 1.0, cex.axis = 1.5)
      mtext("Read Depth",side=2,line=2,cex=0.75,font=2)
      axis(side=2,las=2,tcl=.2)
      labelplot(letter = "", title = "Islet RNA-seq", titlecol = input$islet_color)
      if (highlight_regions) {
        zoomsregion(zoomregion1, highlight = TRUE)
        zoomsregion(zoomregion2, highlight = TRUE)
      } 
    } else {}
    
    # Plot gene annotations
    if (any(choice_endoc %in% "Genes")) {
      id <- which(gtf.res$V1 == chrom & gtf.res$V4 >= chromstart & gtf.res$V5 <= chromend)
      if (length(id) == 0) {
        # determine which element is intersecting
        elm_gr <- GRanges(seqnames = gtf.res$V1, IRanges(start = gtf.res$V4, end = gtf.res$V5))
        inp_gr <- GRanges(seqnames = chrom, IRanges(start = chromstart, end = chromend))
        # find element intersecting
        elm_int <- findOverlaps(query = inp_gr, subject = elm_gr)
        sub_ov <- gtf.res[subjectHits(elm_int),]
        # change element start and end to window
        sub_ov$V4 <- as.numeric(chromstart) + 1
        sub_ov$V5 <- as.numeric(chromend) - 1 
        # plot as usual
        p_genes <- plotGenes(geneinfo = sub_ov,chrom,chromstart,chromend,
                             plotgenetype="box",bentline=FALSE,
                             bheight = 0.1,
                             labeloffset=0.4,fontsize=1,arrowlength = 0.00005,
                             wigglefactor = 0.10, maxrows = 10000,
                             labeltext=TRUE, labelat = "middle")
        labelgenome(chrom, chromstart,chromend,n=4,scale="Mb", scalecex = 1.0, cex.axis = 1.5)
        labelplot(letter = "", title = "Gene Structures")
        if (highlight_regions) {
          zoomsregion(zoomregion1, highlight = TRUE)
          zoomsregion(zoomregion2, highlight = TRUE)
        }
      }
      else {
        gtf.id <- gtf.res[id, ]
        p_genes <- plotGenes(geneinfo = gtf.id,chrom,chromstart,chromend,
                             plotgenetype="box",bentline=FALSE,
                             bheight = 0.1,
                             labeloffset=0.4,fontsize=1,arrowlength = 0.00005,
                             wigglefactor = 0.10, maxrows = 10000,
                             labeltext=TRUE, labelat = "middle")
        labelgenome(chrom, chromstart,chromend,n=4,scale="Mb", scalecex = 1.0, cex.axis = 1.5)
        labelplot(letter = "", title = "Gene Structures")
        if (highlight_regions) {
          zoomsregion(zoomregion1, highlight = TRUE)
          zoomsregion(zoomregion2, highlight = TRUE)
        }
      }
    }
  }
  
  # separate function to output plot to pdf
  generate_plot_pdf <- function(chrom, chromstart, chromend, pair1_start, pair1_end, pair2_start, pair2_end,
                             highlight_regions = TRUE, choice_endoc = c("Hi-C", "ChIA-PET", "Chromatin-States",
                                                                        "ATAC-seq", "RNA-seq", "Genes"),
                             choice_islet = c("Hi-C", "Chromatin-States",
                                              "ATAC-seq", "RNA-seq")) {
    # zoom regions
    zoomregion1 <- c(pair1_start, pair1_end)
    zoomregion2 <- c(pair2_start, pair2_end)
    # plot endoC hic
    if (any(choice_endoc %in% "Hi-C")) {
      p_endoc_hic <- plotBedpe(hic.sel, chrom, chromstart, chromend,
                               color = input$endoc_color, flip = TRUE,
                               plottype="lines", lwd = 1, bty = "n")
      labelplot(letter = "", title = "EndoC Hi-C", titlecol = input$endoc_color)
      labelgenome(chrom,chromstart,chromend,n=4,scale="Mb", scalecex = 1.0, cex.axis = 1.5)
      if (highlight_regions) {
        zoomsregion(zoomregion1, highlight = TRUE)
        zoomsregion(zoomregion2, highlight = TRUE)
      }
    } else {}
    
    # plot islet hic
    if (any(choice_islet %in% "Hi-C")) {
      p_islet_hic <- plotBedpe(islet_hic_sel, chrom, chromstart, chromend,
                               color = input$islet_color, flip = TRUE,
                               plottype="lines", lwd = 1, bty = "n")
      labelplot(letter = "", title = "Islet Hi-C", titlecol = input$islet_color)
      labelgenome(chrom,chromstart,chromend,n=4,scale="Mb", scalecex = 1.0, cex.axis = 1.5)
      if (highlight_regions) {
        zoomsregion(zoomregion1, highlight = TRUE)
        zoomsregion(zoomregion2, highlight = TRUE)
      }
    } else {}
    
    # plot endoC chia-pet
    if (any(choice_endoc %in% "ChIA-PET")) {
      p_endoc_chia <- plotBedpe(chia,chrom,chromstart,chromend,
                                color = input$endoc_color,
                                heights = chia$score,plottype="loops")
      labelplot(letter = "", title = "EndoC ChIA-PET (Pol2)", titlecol = input$endoc_color)
      mtext("Score",side=2,line=2,cex=1,font=2)
      axis(side=2,las=2,tcl=.2)
      labelgenome(chrom,chromstart,chromend,n=4,scale="Mb", scalecex = 1.0, cex.axis = 1.5)
      if (highlight_regions) {
        zoomsregion(zoomregion1, highlight = TRUE)
        zoomsregion(zoomregion2, highlight = TRUE)
      }
    } else {}
    
    # if plot any chromatin states (include legend first)
    if (any(choice_endoc %in% "Chromatin-States") | any(choice_islet %in% " Chromatin-States")) {
      # chromatin state legends
      pchrom <- plotBed(chromhmm_ext[,1:3], chrom = chrom, chromstart = chromstart, chromend = chromend,
                        type = "region", color = "white", row = "supplied", height = 0.01)
      legend("topleft",inset =0.01,legend=c("Weak transcription", "Strong transcription",
                                            "Repressed polycomb", "Weak repressed polycomb",
                                            "Genic enhancer"),
             col=levels(chromhmm_ext$Color)[1:5], pch=19, text.font=2, pt.cex = 2,
             bg = "grey95", cex = 1, horiz = TRUE)
      legend("bottomleft",inset =0.01,legend=c("Poised TSS", "Active TSS",
                                               "Weak TSS", "Active enhancer",
                                               "Weak enhancer", "Low signal/Quiescent"),
             col=levels(chromhmm_ext$Color)[6:11], pch=19, text.font=2, pt.cex = 2,
             bg = "grey95", cex = 1,  horiz = TRUE)
      labelplot(letter = "", title = "Chromatin States (chromHMM) Legend", titlecol = "black")
    }
    
    # plot endoC chromatin states
    if (any(choice_endoc %in% "Chromatin-States")) {
      # error message if there are no bed coordinates to plot
      id_ec <- which(chromhmm_ext$Chr == chrom & chromhmm_ext$Start >= chromstart & chromhmm_ext$End <= chromend)
      if (length(id_ec) == 0) {
        # determine which element is intersecting
        elm_gr <- GRanges(seqnames = chromhmm_ext$Chr, IRanges(start = chromhmm_ext$Start, end = chromhmm_ext$End))
        inp_gr <- GRanges(seqnames = chrom, IRanges(start = chromstart, end = chromend))
        # find element intersecting
        elm_int <- findOverlaps(query = inp_gr, subject = elm_gr)
        sub_ov <- chromhmm_ext[subjectHits(elm_int),]
        # change element start and end to window
        sub_ov$Start <- as.numeric(chromstart) + 1
        sub_ov$End <- as.numeric(chromend) - 1 
        # plot as usual
        p_endoc_chrom <- plotBed(sub_ov[,1:3], chrom = chrom, chromstart = chromstart, chromend = chromend,
                                 type = "region", color = as.character(sub_ov$Color), row = "supplied", height = 0.01, plotbg="grey95")
        labelgenome(chrom, chromstart, chromend, n=4,scale="Mb",edgeblankfraction=0.10, scalecex = 1.0, cex.axis = 1.5)
        labelplot(letter = "", title = "EndoC Chromatin States", titlecol = input$endoc_color)
      }
      else {
        p_endoc_chrom <- plotBed(chromhmm_ext[,1:3], chrom = chrom, chromstart = chromstart, chromend = chromend,
                                 type = "region", color = as.character(chromhmm_ext$Color), row = "supplied", height = 0.01, plotbg="grey95")
        labelgenome(chrom, chromstart, chromend, n=4,scale="Mb",edgeblankfraction=0.10, scalecex = 1.0, cex.axis = 1.5)
        labelplot(letter = "", title = "EndoC Chromatin States", titlecol = input$endoc_color)
      }
    }
    
    # plot islet chromatin states
    if (any(choice_islet %in% "Chromatin-States")) {
      id_ic <- which(islet_chromhmm_ext$Chr == chrom & islet_chromhmm_ext$Start >= chromstart & islet_chromhmm_ext$End <= chromend)
      if (length(id_ic) == 0) {
        # determine which element is intersecting
        elm_gr <- GRanges(seqnames = islet_chromhmm_ext$Chr, IRanges(start = islet_chromhmm_ext$Start, end = islet_chromhmm_ext$End))
        inp_gr <- GRanges(seqnames = chrom, IRanges(start = chromstart, end = chromend))
        # find element intersecting
        elm_int <- findOverlaps(query = inp_gr, subject = elm_gr)
        sub_ov <- islet_chromhmm_ext[subjectHits(elm_int),]
        # change element start and end to window
        sub_ov$Start <- as.numeric(chromstart) + 1
        sub_ov$End <- as.numeric(chromend) - 1
        # plot as usual
        p_endoc_chrom <- plotBed(sub_ov[,1:3], chrom = chrom, chromstart = chromstart, chromend = chromend,
                                 type = "region", color = as.character(sub_ov$Color), row = "supplied", height = 0.01, plotbg="grey95")
        labelgenome(chrom, chromstart, chromend, n=4,scale="Mb",edgeblankfraction=0.10, scalecex = 1.0, cex.axis = 1.5)
        labelplot(letter = "", title = "Islet Chromatin States", titlecol = input$islet_color)
      }
      else {
        p_islet_chrom <- plotBed(islet_chromhmm_ext[,1:3], chrom = chrom, chromstart = chromstart, chromend = chromend,
                                 type = "region", color = as.character(islet_chromhmm_ext$Color), row = "supplied", height = 0.01, plotbg="grey95")
        labelgenome(chrom, chromstart, chromend, n=4,scale="Mb",edgeblankfraction=0.10, scalecex = 1.0, cex.axis = 1.5)
        labelplot(letter = "", title = "Islet Chromatin States", titlecol = input$islet_color)
      }
    }
    
    # plot endoC ATAC-seq
    if (any(choice_endoc %in% "ATAC-seq")) {
      p_endoc_atac <- plotBedgraph(atac,chrom,chromstart,chromend, color = input$endoc_color)
      labelgenome(chrom,chromstart,chromend,n=4,scale="Mb", scalecex = 1.0, cex.axis = 1.5)
      mtext("Read Depth",side=2,line=2,cex=0.75,font=2)
      axis(side=2,las=2,tcl=.2)
      labelplot(letter = "", title = "EndoC ATAC-seq", titlecol = input$endoc_color)
      if (highlight_regions) {
        zoomsregion(zoomregion1, highlight = TRUE)
        zoomsregion(zoomregion2, highlight = TRUE)
      }
    } else {}
    
    # plot islet ATAC-seq
    if (any(choice_islet %in% "ATAC-seq")) {
      p_islet_atac <- plotBedgraph(islet_atac,chrom,chromstart,chromend, color = input$islet_color)
      labelgenome(chrom,chromstart,chromend,n=4,scale="Mb", scalecex = 1.0, cex.axis = 1.5)
      mtext("Read Depth",side=2,line=2,cex=0.75,font=2)
      axis(side=2,las=2,tcl=.2)
      labelplot(letter = "", title = "Islet ATAC-seq", titlecol = input$islet_color)
      if (highlight_regions) {
        zoomsregion(zoomregion1, highlight = TRUE)
        zoomsregion(zoomregion2, highlight = TRUE)
      }
    } else {}
    
    # plot endoc rna-seq
    if (any(choice_endoc %in% "RNA-seq")) {
      p_endoc_rna <- plotBedgraph(rna,chrom,chromstart,chromend, color = input$endoc_color)
      labelgenome(chrom,chromstart,chromend,n=4,scale="Mb", scalecex = 1.0, cex.axis = 1.5)
      mtext("Read Depth",side=2,line=2,cex=0.75,font=2)
      axis(side=2,las=2,tcl=.2)
      labelplot(letter = "", title = "EndoC RNA-seq", titlecol = input$endoc_color)
      if (highlight_regions) {
        zoomsregion(zoomregion1, highlight = TRUE)
        zoomsregion(zoomregion2, highlight = TRUE)
      }
    } else {}
    
    # plot islet rna-seq
    if (any(choice_islet %in% "RNA-seq")) {
      p_islet_rna <- plotBedgraph(islet_rna,chrom,chromstart,chromend, color = input$islet_color)
      labelgenome(chrom,chromstart,chromend,n=4,scale="Mb", scalecex = 1.0, cex.axis = 1.5)
      mtext("Read Depth",side=2,line=2,cex=0.75,font=2)
      axis(side=2,las=2,tcl=.2)
      labelplot(letter = "", title = "Islet RNA-seq", titlecol = input$islet_color)
      if (highlight_regions) {
        zoomsregion(zoomregion1, highlight = TRUE)
        zoomsregion(zoomregion2, highlight = TRUE)
      } 
    } else {}
    
    # Plot gene annotations
    if (any(choice_endoc %in% "Genes")) {
      id <- which(gtf.res$V1 == chrom & gtf.res$V4 >= chromstart & gtf.res$V5 <= chromend)
      if (length(id) == 0) {
        # determine which element is intersecting
        elm_gr <- GRanges(seqnames = gtf.res$V1, IRanges(start = gtf.res$V4, end = gtf.res$V5))
        inp_gr <- GRanges(seqnames = chrom, IRanges(start = chromstart, end = chromend))
        # find element intersecting
        elm_int <- findOverlaps(query = inp_gr, subject = elm_gr)
        sub_ov <- gtf.res[subjectHits(elm_int),]
        # change element start and end to window
        sub_ov$V4 <- as.numeric(chromstart) + 1
        sub_ov$V5 <- as.numeric(chromend) - 1 
        # plot as usual
        p_genes <- plotGenes(geneinfo = sub_ov,chrom,chromstart,chromend,
                             plotgenetype="box",bentline=FALSE,
                             bheight = 0.1,
                             labeloffset=0.4,fontsize=1,arrowlength = 0.00005,
                             wigglefactor = 0.10, maxrows = 10000,
                             labeltext=TRUE, labelat = "middle")
        labelgenome(chrom, chromstart,chromend,n=4,scale="Mb", scalecex = 1.0, cex.axis = 1.5)
        labelplot(letter = "", title = "Gene Structures")
        if (highlight_regions) {
          zoomsregion(zoomregion1, highlight = TRUE)
          zoomsregion(zoomregion2, highlight = TRUE)
        }
      }
      else {
        gtf.id <- gtf.res[id, ]
        p_genes <- plotGenes(geneinfo = gtf.id,chrom,chromstart,chromend,
                             plotgenetype="box",bentline=FALSE,
                             bheight = 0.1,
                             labeloffset=0.4,fontsize=1,arrowlength = 0.00005,
                             wigglefactor = 0.10, maxrows = 10000,
                             labeltext=TRUE, labelat = "middle")
        labelgenome(chrom, chromstart,chromend,n=4,scale="Mb", scalecex = 1.0, cex.axis = 1.5)
        labelplot(letter = "", title = "Gene Structures")
        if (highlight_regions) {
          zoomsregion(zoomregion1, highlight = TRUE)
          zoomsregion(zoomregion2, highlight = TRUE)
        }
      }
    }
  }
  
  # shiny app error validation for plots
  valid_plot <- function() {
    validate(
      need(isolate(input$chrom), "Please provide a chromosome number!"),
      need(isolate(input$chrom %in% paste("chr", 1:22, sep = "")), "Please specify a valid autosomal chromosome number!"),
      need(isolate(input$chromstart), "Start coordinate is missing!"),
      need(isolate(input$chromstart >= 0), "Start coordinate cannot be less than 0!"),
      need(isolate(input$chromend), "End coordinate is missing!"),
      need(isolate(input$chromend >= 0), "End coordinate cannot be less than 0!"),
      need(isolate(input$chromend != input$chromstart), "Start and end coordinates cannot be equal"),
      need(isolate(input$chromend > input$chromstart), "End coordinate must be greater start coordinate")
    )
  }
  
  # shiny app error validation for tables
  valid_data <- function() {
    validate(
      need(input$chrom, "Please provide a chromosome number!"),
      need(input$chrom %in% paste("chr", 1:22, sep = ""), "Please specify a valid autosomal chromosome number!"),
      need(input$chromstart, "Start coordinate is missing!"),
      need(input$chromstart >= 0, "Start coordinate cannot be less than 0!"),
      need(input$chromend, "End coordinate is missing!"),
      need(input$chromend >= 0, "End coordinate cannot be less than 0!")
    )
  }
  
  ##### function to extract gene names
  extract_gene <- function(gene_symbol) {
    # find indexes of gene symbol
    idx <- which(gtf.res$V9 == toupper(gene_symbol))
    idx <- idx[1]
    # return index location
    return(idx)
  }
  
  # get coordinates for a gene symbol
  observeEvent(input$do_gene, {
    idx <- extract_gene(gene_symbol = input$gene)
    # update chr, start, and end
    updateTextInput(session, inputId = "chrom", label = NULL,
                    value = as.character(gtf.res$V1[idx]))
    updateNumericInput(session, inputId = "chromstart", label = NULL,
                       value = as.numeric(gtf.res$V4[idx])-2000)
    updateNumericInput(session, inputId = "chromend", label = NULL,
                       value = as.numeric(gtf.res$V5[idx])+2000)
  })
  
  # make a function for plotting output to panel, updating plot (with zoom buttons)
  plotout <- function() {
    # does user want to highlight regions?
    if (isolate(input$highlight) == "Yes") {
      var_high <- TRUE
    } else {
      var_high <- FALSE
    }
    generate_plot1(chrom = as.character(isolate(input$chrom)),
                   chromstart = as.numeric(isolate(input$chromstart)),
                   chromend = as.numeric(isolate(input$chromend)),
                   pair1_start = as.numeric(isolate(input$pair1_start)), 
                   pair1_end = as.numeric(isolate(input$pair1_end)),
                   pair2_start = as.numeric(isolate(input$pair2_start)),
                   pair2_end = as.numeric(isolate(input$pair2_end)),
                   highlight_regions = var_high,
                   choice_endoc = isolate(input$EndoCData),
                   choice_islet = isolate(input$IsletData))
  }
  
  # make a function for plotting output to pdf
  plotout_pdf <- function() {
    # does user want to highlight regions?
    if (isolate(input$highlight) == "Yes") {
      var_high <- TRUE
    } else {
      var_high <- FALSE
    }
    generate_plot_pdf(chrom = as.character(isolate(input$chrom)),
                   chromstart = as.numeric(isolate(input$chromstart)),
                   chromend = as.numeric(isolate(input$chromend)),
                   pair1_start = as.numeric(isolate(input$pair1_start)), 
                   pair1_end = as.numeric(isolate(input$pair1_end)),
                   pair2_start = as.numeric(isolate(input$pair2_start)),
                   pair2_end = as.numeric(isolate(input$pair2_end)),
                   highlight_regions = var_high,
                   choice_endoc = isolate(input$EndoCData),
                   choice_islet = isolate(input$IsletData))
  }
  
  # for zooming out 1.5 times, take the end minus start, multiply by 1.5
  # don't let coordinate values become less than 0
  # zoom out 1.5 X
  observeEvent(input$zoom_out_1.5, {
    valid_plot()
    # calculate diff between end and start
    distance <- input$chromend - input$chromstart
    add_distance <- round((1.5 * distance)/2, digits = 0)
    # new start and end distance
    new_start <- as.numeric(input$chromstart - add_distance)
    if (new_start < 0) {
      new_start <- 0
    } else {}
    new_end <- as.numeric(input$chromend + add_distance)
    # add substract distance from start, add to end
    updateNumericInput(session, inputId = "chromstart", label = NULL,
                       value = new_start)
    updateNumericInput(session, inputId = "chromend", label = NULL,
                       value = new_end)
    output$endocPlot <- renderPlot({
      par(mfrow=c(11,1), mar=c(2,4,2,2))
      if (isolate(input$highlight) == "Yes") {
        var_high <- TRUE
      } else {
        var_high <- FALSE
      }
      withProgress(expr = generate_plot1(chrom = as.character(isolate(input$chrom)),
                                         chromstart = as.numeric(isolate(new_start)),
                                         chromend = as.numeric(isolate(new_end)),
                                         pair1_start = as.numeric(isolate(input$pair1_start)), 
                                         pair1_end = as.numeric(isolate(input$pair1_end)),
                                         pair2_start = as.numeric(isolate(input$pair2_start)),
                                         pair2_end = as.numeric(isolate(input$pair2_end)),
                                         highlight_regions = var_high,
                                         choice_endoc = isolate(input$EndoCData),
                                         choice_islet = isolate(input$IsletData)), 
                   message = "Updating multiomics plot, please wait")
    })
  })
  
  # zoom out 3X
  observeEvent(input$zoom_out_3, {
    valid_plot()
    # calculate diff between end and start
    distance <- input$chromend - input$chromstart
    add_distance <- round((3.0 * distance)/2, digits = 0)
    # new start distance
    new_start <- as.numeric(input$chromstart - add_distance)
    if (new_start < 0) {
      new_start <- 0
    } else {}
    new_end <- as.numeric(input$chromend + add_distance)
    # add substract distance from start, add to end
    updateNumericInput(session, inputId = "chromstart", label = NULL,
                       value = new_start)
    updateNumericInput(session, inputId = "chromend", label = NULL,
                       value = new_end)
    output$endocPlot <- renderPlot({
      par(mfrow=c(11,1), mar=c(2,4,2,2))
      if (isolate(input$highlight) == "Yes") {
        var_high <- TRUE
      } else {
        var_high <- FALSE
      }
      withProgress(expr = generate_plot1(chrom = as.character(isolate(input$chrom)),
                                         chromstart = as.numeric(isolate(new_start)),
                                         chromend = as.numeric(isolate(new_end)),
                                         pair1_start = as.numeric(isolate(input$pair1_start)), 
                                         pair1_end = as.numeric(isolate(input$pair1_end)),
                                         pair2_start = as.numeric(isolate(input$pair2_start)),
                                         pair2_end = as.numeric(isolate(input$pair2_end)),
                                         highlight_regions = var_high,
                                         choice_endoc = isolate(input$EndoCData),
                                         choice_islet = isolate(input$IsletData)), 
                   message = "Updating multiomics plot, please wait")
    })
  })
  
  # zoom in 1.5X
  observeEvent(input$zoom_in_1.5, {
    valid_plot()
    # calculate diff between end and start
    distance <- input$chromend - input$chromstart
    add_distance <- round((distance / 1.5)/2, digits = 0)
    # new start distance
    new_start <- as.numeric(input$chromstart + add_distance)
    if (new_start < 0) {
      new_start <- 0
    } else {}
    new_end <- as.numeric(input$chromend - add_distance)
    # add substract distance from start, add to end
    updateNumericInput(session, inputId = "chromstart", label = NULL,
                       value = new_start)
    updateNumericInput(session, inputId = "chromend", label = NULL,
                       value = new_end)
    output$endocPlot <- renderPlot({
      par(mfrow=c(11,1), mar=c(2,4,2,2))
      if (isolate(input$highlight) == "Yes") {
        var_high <- TRUE
      } else {
        var_high <- FALSE
      }
      withProgress(expr = generate_plot1(chrom = as.character(isolate(input$chrom)),
                                         chromstart = as.numeric(isolate(new_start)),
                                         chromend = as.numeric(isolate(new_end)),
                                         pair1_start = as.numeric(isolate(input$pair1_start)), 
                                         pair1_end = as.numeric(isolate(input$pair1_end)),
                                         pair2_start = as.numeric(isolate(input$pair2_start)),
                                         pair2_end = as.numeric(isolate(input$pair2_end)),
                                         highlight_regions = var_high,
                                         choice_endoc = isolate(input$EndoCData),
                                         choice_islet = isolate(input$IsletData)), 
                   message = "Updating multiomics plot, please wait")
    })
  })
  
  # zoom in 3X
  observeEvent(input$zoom_in_3, {
    valid_plot()
    # calculate diff between end and start
    distance <- input$chromend - input$chromstart
    add_distance <- round((distance / 3.0)/2, digits = 0)
    # new start distance
    new_start <- as.numeric(input$chromstart + add_distance)
    if (new_start < 0) {
      new_start <- 0
    } else {}
    new_end <- as.numeric(input$chromend - add_distance)
    # add substract distance from start, add to end
    updateNumericInput(session, inputId = "chromstart", label = NULL,
                       value = new_start)
    updateNumericInput(session, inputId = "chromend", label = NULL,
                       value = new_end)
    output$endocPlot <- renderPlot({
      par(mfrow=c(11,1), mar=c(2,4,2,2))
      if (isolate(input$highlight) == "Yes") {
        var_high <- TRUE
      } else {
        var_high <- FALSE
      }
      withProgress(expr = generate_plot1(chrom = as.character(isolate(input$chrom)),
                                         chromstart = as.numeric(isolate(new_start)),
                                         chromend = as.numeric(isolate(new_end)),
                                         pair1_start = as.numeric(isolate(input$pair1_start)), 
                                         pair1_end = as.numeric(isolate(input$pair1_end)),
                                         pair2_start = as.numeric(isolate(input$pair2_start)),
                                         pair2_end = as.numeric(isolate(input$pair2_end)),
                                         highlight_regions = var_high,
                                         choice_endoc = isolate(input$EndoCData),
                                         choice_islet = isolate(input$IsletData)), 
                   message = "Updating multiomics plot, please wait")
    })
  })
  
  # submit button to plot coordinates, if no coords provided, give error
  observeEvent(input$do, {
    # Generate a plot of the requested chrom, start, end
    output$endocPlot <- renderPlot({
      valid_plot()
      par(mfrow=c(11,1), mar=c(2,4,2,2))
      withProgress(expr = plotout(), message = "Generating multiomics plot, please wait")
    })
  })
  
  ########################################################################################################################
  # interactive plot stuff to get endoc chromatin states, can make a separate panel for each relevant dataset
  ########################################################################################################################
  
  #### function to return table of snps (and selected snps)
  xy_range_snps <- function(e) {
    valid_data()
    if (is.null(e)) {
      emin <- input$chromstart
      emax <- input$chromend
    } else {
      emin <- round(e$xmin, digits = 0)
      emax <- round(e$xmax, digits = 0)
    }
    id.snp <- which(snps.df$Chromosome == input$chrom & snps.df$Start >= emin & snps.df$End <= emax)
    snps.res <- snps.df[id.snp,]
    return(snps.res)
  }
  
  ##### function to extract coordinates of endoC and islet chia-pet, hi-c loops that intersect the region of interest
  # function takes coordinates as input, returns a data frame of endoc chia-pet, endoc hic, and islet hic coordinates
  overlap_loops <- function(chrom, chromstart, chromend) {
    valid_data()
    gr_in <- GRanges(seqnames = chrom, IRanges(start = chromstart, end = chromend))
    # make endoc, islet granges
    endoc_chia1 <- GRanges(seqnames = chia$chrom1, IRanges(start = chia$start1, end = chia$end1))
    endoc_chia2 <- GRanges(seqnames = chia$chrom2, IRanges(start = chia$start2, end = chia$end2))
    endoc_chia_gi <- GInteractions(endoc_chia1, endoc_chia2)
    endoc_hic1 <- GRanges(seqnames = hic.sel$chrom1, IRanges(start = hic.sel$start1, end = hic.sel$end1))
    endoc_hic2 <- GRanges(seqnames = hic.sel$chrom2, IRanges(start = hic.sel$start2, end = hic.sel$end2))
    endoc_hic_gi <- GInteractions(endoc_hic1, endoc_hic2)
    islet_hic1 <- GRanges(seqnames = islet_hic_sel$chrom1, IRanges(start = islet_hic_sel$start1, end = islet_hic_sel$end1))
    islet_hic2 <- GRanges(seqnames = islet_hic_sel$chrom2, IRanges(start = islet_hic_sel$start2, end = islet_hic_sel$end2))
    islet_hic_gi <- GInteractions(islet_hic1, islet_hic2)
    # perform an overlap analysis
    ov_endoc_chia <- findOverlaps(gr_in, endoc_chia_gi)
    ov_endoc_hic <- findOverlaps(gr_in, endoc_hic_gi)
    ov_islet_hic <- findOverlaps(gr_in, islet_hic_gi)
    # combine all results in a data frame
    res_endoc_chia <- as.data.frame(endoc_chia_gi[subjectHits(ov_endoc_chia)])
    Data_Type <- rep("EndoC_ChIA_PET", nrow(res_endoc_chia))
    res_endoc_chia <- cbind(res_endoc_chia, Data_Type)
    res_endoc_hic <- as.data.frame(endoc_hic_gi[subjectHits(ov_endoc_hic)])
    Data_Type <- rep("EndoC_HiC", nrow(res_endoc_hic))
    res_endoc_hic <- cbind(res_endoc_hic, Data_Type)
    res_islet_hic <- as.data.frame(islet_hic_gi[subjectHits(ov_islet_hic)])
    Data_Type <- rep("Islet_HiC", nrow(res_islet_hic))
    res_islet_hic <- cbind(res_islet_hic, Data_Type)
    # merge all together
    all_ov <- rbind(res_endoc_chia, res_endoc_hic, res_islet_hic)
    all_ov <- all_ov[, c(1:4, 6:9, 11)]
    # find genes overlapping, get tss coordinates
    plus.id <- which(gtf.res$V7 == "+")
    neg.id <- which(gtf.res$V7 == "-")
    gtf.tss <- gtf.res
    gtf.tss$V5[plus.id] <- gtf.tss$V4[plus.id]+1
    gtf.tss$V4[neg.id] <- gtf.tss$V5[neg.id]-1
    genes_gr <- GRanges(seqnames = gtf.tss$V1, IRanges(start = gtf.tss$V4, end = gtf.tss$V5), strand = gtf.tss$V7)
    mcols(genes_gr)$Gene <- gtf.tss$V9
    ov_gr1 <- GRanges(seqnames = all_ov$seqnames1, IRanges(start = all_ov$start1, end = all_ov$end1))
    ov_gr2 <- GRanges(seqnames = all_ov$seqnames2, IRanges(start = all_ov$start2, end = all_ov$end2))
    # find nearest gene
    ov_gr1_nr <- GenomicRanges::distanceToNearest(ov_gr1, genes_gr)
    ov_gr1_genes <- genes_gr[subjectHits(ov_gr1_nr)]$Gene
    ov_gr2_nr <- GenomicRanges::distanceToNearest(ov_gr2, genes_gr)
    ov_gr2_genes <- genes_gr[subjectHits(ov_gr2_nr)]$Gene
    all_ext <- cbind(all_ov[,1:4], ov_gr1_genes, ov_gr1_nr@elementMetadata$distance, all_ov[, 5:8], ov_gr2_genes, ov_gr2_nr@elementMetadata$distance, all_ov[,9])
    colnames(all_ext)[c(1,5:6, 11:13)] <- c("Chromosome", "Gene1", "Distance to Gene1 TSS", "Gene2", "Distance to Gene2 TSS", "Data Type")
    # change back width to 0 based index for BED
    all_ext[,4] <- as.numeric(all_ext[,4]) - 1
    return(all_ext)
  }
  
  # selected endoc chromatin states
  xy_range_chromhmm_endo <- function(e) {
    shiny::validate(
      need(any(input$EndoCData %in% "Chromatin-States"), "EndoC-βH1 Chromatin States data type is not selected")
    )
    valid_data()
    # if data type is selected, render table
    if (any(input$EndoCData %in% "Chromatin-States")) {
      if (is.null(e)) {
        emin <- input$chromstart
        emax <- input$chromend
      } else {
        emin <- round(e$xmin, digits = 0)
        emax <- round(e$xmax, digits = 0)
      }
      gr_high <- GRanges(seqnames = input$chrom, IRanges(start = emin, end = emax))
      c_idx <- which(chromhmm_ext[,1] %in% input$chrom) 
      gr_ch_endo <- GRanges(seqnames = chromhmm_ext[c_idx,1], IRanges(start = chromhmm_ext[c_idx,2], end = chromhmm_ext[c_idx,3]))
      mcols(gr_ch_endo)$State <- chromhmm_ext$State[c_idx]
      mcols(gr_ch_endo)$Color <- chromhmm_ext$Color[c_idx]
      # find overlaps
      gr_ov <- findOverlaps(gr_high, gr_ch_endo)
      # get data frame of intersect
      endo_ch_df <- as.data.frame(gr_ch_endo[subjectHits(gr_ov)])
      outdf <- endo_ch_df[, c(1:4, 6:7)]
      outdf <- as.matrix(outdf)
      # assign color codes
      id1 <- which(outdf[,6] == "#006400")
      id2 <- which(outdf[,6] == "#008000")
      id3 <- which(outdf[,6] == "#808080")
      id4 <- which(outdf[,6] == "#c0c0c0")
      id5 <- which(outdf[,6] == "#c2e105")
      id6 <- which(outdf[,6] == "#cd5c5c")
      id7 <- which(outdf[,6] == "#ff0000")
      id8 <- which(outdf[,6] == "#ff4500")
      id9 <- which(outdf[,6] == "#ffc34d")
      id10 <- which(outdf[,6] == "#ffff00")
      id11 <- which(outdf[,6] == "#ffffff")
      outdf[id1, 6] <- rep('<img src="http://www.htmlcsscolor.com/preview/16x16/006400.png"></img>', length(id1))
      outdf[id2, 6] <- rep('<img src="http://www.htmlcsscolor.com/preview/16x16/008000.png"></img>', length(id2))
      outdf[id3, 6] <- rep('<img src="http://www.htmlcsscolor.com/preview/16x16/808080.png"></img>', length(id3))
      outdf[id4, 6] <- rep('<img src="http://www.htmlcsscolor.com/preview/16x16/c0c0c0.png"></img>', length(id4))
      outdf[id5, 6] <- rep('<img src="http://www.htmlcsscolor.com/preview/16x16/c2e105.png"></img>', length(id5))
      outdf[id6, 6] <- rep('<img src="http://www.htmlcsscolor.com/preview/16x16/cd5c5c.png"></img>', length(id6))
      outdf[id7, 6] <- rep('<img src="http://www.htmlcsscolor.com/preview/16x16/ff0000.png"></img>', length(id7))
      outdf[id8, 6] <- rep('<img src="http://www.htmlcsscolor.com/preview/16x16/ff4500.png"></img>', length(id8))
      outdf[id9, 6] <- rep('<img src="http://www.htmlcsscolor.com/preview/16x16/ffc34d.png"></img>', length(id9))
      outdf[id10, 6] <- rep('<img src="http://www.htmlcsscolor.com/preview/16x16/ffff00.png"></img>', length(id10))
      outdf[id11, 6] <- rep('<img src="http://www.htmlcsscolor.com/preview/16x16/ffffff.png"></img>', length(id11))
      endoc_chrom <- as.data.frame(outdf)
      colnames(endoc_chrom)[1] <- "Chromosome"
      # change back width to 0 based index for BED
      endoc_chrom[,4] <- as.numeric(as.character(endoc_chrom[,4])) - 1
      return(endoc_chrom)
    }
  }
  # endoc chromatin states tab
  output$info <- DT::renderDataTable({
    endoc_ch <- xy_range_chromhmm_endo(input$plot_brush)
    DT::datatable(endoc_ch, escape = FALSE)
  })
  
  # selected islet chromatin states
  xy_range_chromhmm_islet <- function(e) {
    shiny::validate(
      need(any(input$IsletData %in% "Chromatin-States"), "Islet Chromatin States data type is not selected")
    )
    valid_data()
    # if data type is selected, render table
    if (any(input$IsletData %in% "Chromatin-States")) {
      if (is.null(e)) {
        emin <- input$chromstart
        emax <- input$chromend
      } else {
        emin <- round(e$xmin, digits = 0)
        emax <- round(e$xmax, digits = 0)
      }
      gr_high <- GRanges(seqnames = input$chrom, IRanges(start = emin, end = emax))
      gr_ch_endo <- GRanges(seqnames = islet_chromhmm_ext[,1], IRanges(start = islet_chromhmm_ext[,2], end = islet_chromhmm_ext[,3]))
      mcols(gr_ch_endo)$State <- islet_chromhmm_ext$State
      mcols(gr_ch_endo)$Color <- islet_chromhmm_ext$Color
      # find overlaps
      gr_ov <- findOverlaps(gr_high, gr_ch_endo)
      # get data frame of intersect
      endo_ch_df <- as.data.frame(gr_ch_endo[subjectHits(gr_ov)])
      outdf <- endo_ch_df[, c(1:4, 6:7)]
      outdf <- as.matrix(outdf)
      # assign color codes
      id1 <- which(outdf[,6] == "#006400")
      id2 <- which(outdf[,6] == "#008000")
      id3 <- which(outdf[,6] == "#808080")
      id4 <- which(outdf[,6] == "#c0c0c0")
      id5 <- which(outdf[,6] == "#c2e105")
      id6 <- which(outdf[,6] == "#cd5c5c")
      id7 <- which(outdf[,6] == "#ff0000")
      id8 <- which(outdf[,6] == "#ff4500")
      id9 <- which(outdf[,6] == "#ffc34d")
      id10 <- which(outdf[,6] == "#ffff00")
      id11 <- which(outdf[,6] == "#ffffff")
      outdf[id1, 6] <- rep('<img src="http://www.htmlcsscolor.com/preview/16x16/006400.png"></img>', length(id1))
      outdf[id2, 6] <- rep('<img src="http://www.htmlcsscolor.com/preview/16x16/008000.png"></img>', length(id2))
      outdf[id3, 6] <- rep('<img src="http://www.htmlcsscolor.com/preview/16x16/808080.png"></img>', length(id3))
      outdf[id4, 6] <- rep('<img src="http://www.htmlcsscolor.com/preview/16x16/c0c0c0.png"></img>', length(id4))
      outdf[id5, 6] <- rep('<img src="http://www.htmlcsscolor.com/preview/16x16/c2e105.png"></img>', length(id5))
      outdf[id6, 6] <- rep('<img src="http://www.htmlcsscolor.com/preview/16x16/cd5c5c.png"></img>', length(id6))
      outdf[id7, 6] <- rep('<img src="http://www.htmlcsscolor.com/preview/16x16/ff0000.png"></img>', length(id7))
      outdf[id8, 6] <- rep('<img src="http://www.htmlcsscolor.com/preview/16x16/ff4500.png"></img>', length(id8))
      outdf[id9, 6] <- rep('<img src="http://www.htmlcsscolor.com/preview/16x16/ffc34d.png"></img>', length(id9))
      outdf[id10, 6] <- rep('<img src="http://www.htmlcsscolor.com/preview/16x16/ffff00.png"></img>', length(id10))
      outdf[id11, 6] <- rep('<img src="http://www.htmlcsscolor.com/preview/16x16/ffffff.png"></img>', length(id11))
      islet_chrom <- as.data.frame(outdf)
      colnames(islet_chrom)[1] <- "Chromosome"
      # change back width to 0 based index for BED
      islet_chrom[,4] <- as.numeric(as.character(islet_chrom[,4])) - 1
      return(islet_chrom)
    }
  }
  
  # islet chromatin states tab
  output$islet <- DT::renderDataTable({
    islet_ch <- xy_range_chromhmm_islet(input$plot_brush)
    DT::datatable(islet_ch, escape = FALSE)
  })
  
  # indicate to user which coordinates were selected
  output$select <- renderText({
    valid_data()
    xy_range_str <- function(e) {
      if(is.null(e)) {
        return("No coordinates selected! Please click and drag mouse over plot in the 'Multiomic Plot' tab to select specific coordinates\n")
      } 
      # the x coordinates of the brush would just be the chromosome start and end positions
      # extract data frame for coordinates, e$xmin is a numeric number
      emin <- round(e$xmin, digits = 0)
      emax <- round(e$xmax, digits = 0)
      # output information to panel
      print(paste("Chromosome: ", input$chrom, "\n",
                  "Selected Starting position: ", emin, "\n",
                  "Selected Ending position: ", emax, sep = ""))
    }
    xy_range_str(input$plot_brush)
  })
  
  # function to retreive selected coordinates
  get_coords <- function(e) {
    valid_data()
    if(is.null(e)) {
      emin <- input$chromstart
      emax <- input$chromend
      vec_coords <- c(emin, emax)
      return(vec_coords)
    } 
    # the x coordinates of the brush would just be the chromosome start and end positions
    # extract data frame for coordinates, e$xmin is a numeric number
    emin <- round(e$xmin, digits = 0)
    emax <- round(e$xmax, digits = 0)
    vec_coords <- c(emin, emax)
    return(vec_coords)
  }
  
  # endoc chia-pet selected
  xy_range_chia <- function(e) {
    shiny::validate(
      need(any(input$EndoCData %in% "ChIA-PET"), "EndoC-βH1 ChIA-PET data type is not selected")
    )
    valid_data()
    if (any(input$EndoCData %in% "ChIA-PET")) {
      if (is.null(e)) {
        emin <- input$chromstart
        emax <- input$chromend
      } else {
        emin <- round(e$xmin, digits = 0)
        emax <- round(e$xmax, digits = 0)
      }
      gr_high <- GRanges(seqnames = input$chrom, IRanges(start = emin, end = emax))
      gr_chia1 <- GRanges(seqnames = chia$chrom1, IRanges(start = chia$start1, end = chia$end1))
      gr_chia2 <- GRanges(seqnames = chia$chrom2, IRanges(start = chia$start2, end = chia$end2))
      gr_chia_gi <- GInteractions(gr_chia1, gr_chia2)
      # find overlaps
      gr_ov <- findOverlaps(gr_chia_gi, gr_high)
      # get data frame of intersect
      endo_ch_df <- as.data.frame(gr_chia_gi[queryHits(gr_ov)])
      outdf <- endo_ch_df[, c(1:4, 6:9)]
      # find genes overlapping
      plus.id <- which(gtf.res$V7 == "+")
      neg.id <- which(gtf.res$V7 == "-")
      gtf.tss <- gtf.res
      gtf.tss$V5[plus.id] <- gtf.tss$V4[plus.id]+1
      gtf.tss$V4[neg.id] <- gtf.tss$V5[neg.id]-1
      genes_gr <- GRanges(seqnames = gtf.tss$V1, IRanges(start = gtf.tss$V4, end = gtf.tss$V5), strand = gtf.tss$V7)
      mcols(genes_gr)$Gene <- gtf.tss$V9
      ov_gr1 <- GRanges(seqnames = outdf$seqnames1, IRanges(start = outdf$start1, end = outdf$end1))
      ov_gr2 <- GRanges(seqnames = outdf$seqnames2, IRanges(start = outdf$start2, end = outdf$end2))
      # find nearest gene
      ov_gr1_nr <- GenomicRanges::distanceToNearest(ov_gr1, genes_gr)
      ov_gr1_genes <- genes_gr[subjectHits(ov_gr1_nr)]$Gene
      ov_gr2_nr <- GenomicRanges::distanceToNearest(ov_gr2, genes_gr)
      ov_gr2_genes <- genes_gr[subjectHits(ov_gr2_nr)]$Gene
      outdf_ext <- cbind(outdf[,1:4], ov_gr1_genes, ov_gr1_nr@elementMetadata$distance, outdf[, 5:8], ov_gr2_genes, ov_gr2_nr@elementMetadata$distance)
      colnames(outdf_ext)[c(1, 5:6, 11:12)] <- c("Chromosome", "Gene1", "Distance to Gene1 TSS", "Gene2", "Distance to Gene2 TSS")
      # change back width to 0 based index for BED
      outdf_ext[,4] <- as.numeric(outdf_ext[,4]) - 1
      return(outdf_ext)
    }
  }
  
  # endoc chia-pet tab
  output$chia <- DT::renderDataTable({
    endoc_chia <- xy_range_chia(input$plot_brush)
    DT::datatable(endoc_chia, escape = FALSE)
  })
  
  # endoC selected Hi-C
  xy_range_endohic <- function(e) {
    shiny::validate(
      need(any(input$EndoCData %in% "Hi-C"), "EndoC-βH1 Hi-C data type is not selected")
    )
    valid_data()
    if (any(input$EndoCData %in% "Hi-C")) {
      if (is.null(e)) {
        emin <- input$chromstart
        emax <- input$chromend
      } else {
        emin <- round(e$xmin, digits = 0)
        emax <- round(e$xmax, digits = 0)
      }
      gr_high <- GRanges(seqnames = input$chrom, IRanges(start = emin, end = emax))
      gr_chia1 <- GRanges(seqnames = hic.sel$chrom1, IRanges(start = hic.sel$start1, end = hic.sel$end1))
      gr_chia2 <- GRanges(seqnames = hic.sel$chrom2, IRanges(start = hic.sel$start2, end = hic.sel$end2))
      gr_chia_gi <- GInteractions(gr_chia1, gr_chia2)
      # find overlaps
      gr_ov <- findOverlaps(gr_chia_gi, gr_high)
      # get data frame of intersect
      endo_ch_df <- as.data.frame(gr_chia_gi[queryHits(gr_ov)])
      outdf <- endo_ch_df[, c(1:4, 6:9)]
      # find genes overlapping
      plus.id <- which(gtf.res$V7 == "+")
      neg.id <- which(gtf.res$V7 == "-")
      gtf.tss <- gtf.res
      gtf.tss$V5[plus.id] <- gtf.tss$V4[plus.id]+1
      gtf.tss$V4[neg.id] <- gtf.tss$V5[neg.id]-1
      genes_gr <- GRanges(seqnames = gtf.tss$V1, IRanges(start = gtf.tss$V4, end = gtf.tss$V5), strand = gtf.tss$V7)
      mcols(genes_gr)$Gene <- gtf.tss$V9
      ov_gr1 <- GRanges(seqnames = outdf$seqnames1, IRanges(start = outdf$start1, end = outdf$end1))
      ov_gr2 <- GRanges(seqnames = outdf$seqnames2, IRanges(start = outdf$start2, end = outdf$end2))
      # find nearest gene
      ov_gr1_nr <- GenomicRanges::distanceToNearest(ov_gr1, genes_gr)
      ov_gr1_genes <- genes_gr[subjectHits(ov_gr1_nr)]$Gene
      ov_gr2_nr <- GenomicRanges::distanceToNearest(ov_gr2, genes_gr)
      ov_gr2_genes <- genes_gr[subjectHits(ov_gr2_nr)]$Gene
      outdf_ext <- cbind(outdf[,1:4], ov_gr1_genes, ov_gr1_nr@elementMetadata$distance, outdf[, 5:8], ov_gr2_genes, ov_gr2_nr@elementMetadata$distance)
      colnames(outdf_ext)[c(1, 5:6, 11:12)] <- c("Chromosome", "Gene1", "Distance to Gene1 TSS", "Gene2", "Distance to Gene2 TSS")
      # change back width to 0 based index for BED
      outdf_ext[,4] <- as.numeric(outdf_ext[,4]) - 1
      return(outdf_ext)
    }
  }
  
  # endoc hic tab
  output$endohic <- DT::renderDataTable({
    endohic <- xy_range_endohic(input$plot_brush)
    DT::datatable(endohic, escape = FALSE)
  })
  
  # islet hi-c selected
  xy_range_islethic <- function(e) {
    shiny::validate(
      need(any(input$IsletData %in% "Hi-C"), "Islet Hi-C data type is not selected")
    )
    valid_data()
    if (any(input$IsletData %in% "Hi-C")) {
      if (is.null(e)) {
        emin <- input$chromstart
        emax <- input$chromend
      } else {
        emin <- round(e$xmin, digits = 0)
        emax <- round(e$xmax, digits = 0)
      }
      gr_high <- GRanges(seqnames = input$chrom, IRanges(start = emin, end = emax))
      gr_chia1 <- GRanges(seqnames = islet_hic_sel$chrom1, IRanges(start = islet_hic_sel$start1, end = islet_hic_sel$end1))
      gr_chia2 <- GRanges(seqnames = islet_hic_sel$chrom2, IRanges(start = islet_hic_sel$start2, end = islet_hic_sel$end2))
      gr_chia_gi <- GInteractions(gr_chia1, gr_chia2)
      # find overlaps
      gr_ov <- findOverlaps(gr_chia_gi, gr_high)
      # get data frame of intersect
      endo_ch_df <- as.data.frame(gr_chia_gi[queryHits(gr_ov)])
      outdf <- endo_ch_df[, c(1:4, 6:9)]
      # find genes overlapping
      plus.id <- which(gtf.res$V7 == "+")
      neg.id <- which(gtf.res$V7 == "-")
      gtf.tss <- gtf.res
      gtf.tss$V5[plus.id] <- gtf.tss$V4[plus.id]+1
      gtf.tss$V4[neg.id] <- gtf.tss$V5[neg.id]-1
      genes_gr <- GRanges(seqnames = gtf.tss$V1, IRanges(start = gtf.tss$V4, end = gtf.tss$V5), strand = gtf.tss$V7)
      mcols(genes_gr)$Gene <- gtf.tss$V9
      ov_gr1 <- GRanges(seqnames = outdf$seqnames1, IRanges(start = outdf$start1, end = outdf$end1))
      ov_gr2 <- GRanges(seqnames = outdf$seqnames2, IRanges(start = outdf$start2, end = outdf$end2))
      # find nearest gene
      ov_gr1_nr <- GenomicRanges::distanceToNearest(ov_gr1, genes_gr)
      ov_gr1_genes <- genes_gr[subjectHits(ov_gr1_nr)]$Gene
      ov_gr2_nr <- GenomicRanges::distanceToNearest(ov_gr2, genes_gr)
      ov_gr2_genes <- genes_gr[subjectHits(ov_gr2_nr)]$Gene
      outdf_ext <- cbind(outdf[,1:4], ov_gr1_genes, ov_gr1_nr@elementMetadata$distance, outdf[, 5:8], ov_gr2_genes, ov_gr2_nr@elementMetadata$distance)
      colnames(outdf_ext)[c(1, 5:6, 11:12)] <- c("Chromosome", "Gene1", "Distance to Gene1 TSS", "Gene2", "Distance to Gene2 TSS")
      # change back width to 0 based index for BED
      outdf_ext[,4] <- as.numeric(outdf_ext[,4]) - 1
      return(outdf_ext)
    }
  }
  # islet hi-c tab
  output$islethic <- DT::renderDataTable({
    islet_hic <- xy_range_islethic(input$plot_brush)
    DT::datatable(islet_hic, escape = FALSE)
  })
  
  # endoc atac-seq peaks
  xy_range_endoc_peaks <- function(e) {
    shiny::validate(
      need(any(input$EndoCData %in% "ATAC-seq"), "EndoC-βH1 ATAC-seq data type is not selected")
    )
    valid_data()
    if (any(input$EndoCData %in% "ATAC-seq")) {
      if (is.null(e)) {
        emin <- input$chromstart
        emax <- input$chromend
      } else {
        emin <- round(e$xmin, digits = 0)
        emax <- round(e$xmax, digits = 0)
      }
      gr_high <- GRanges(seqnames = input$chrom, IRanges(start = emin, end = emax))
      gr_atac <- GRanges(seqnames = peaks$Chr, IRanges(start = peaks$Start, end = peaks$End))
      elementMetadata(gr_atac) <- peaks[, 4:ncol(peaks)]
      # find overlaps
      gr_ov <- findOverlaps(gr_atac, gr_high)
      # get data frame of intersect
      outdf <- as.data.frame(gr_atac[queryHits(gr_ov)])
      outdf_ext <- outdf[, c(1:4, 6:14)]
      colnames(outdf_ext)[1] <- "Chromosome"
      # change back width to 0 based index for BED
      outdf_ext[,4] <- as.numeric(outdf_ext[,4]) - 1
      return(outdf_ext)
    }
  }
  
  # endoc atac-seq peaks
  output$endocatac <- DT::renderDataTable({
    endoc_peaks <- xy_range_endoc_peaks(input$plot_brush)
    DT::datatable(endoc_peaks, escape = FALSE)
  })
  
  # islet atac-seq peaks
  xy_range_islet_peaks <- function(e) {
    shiny::validate(
      need(any(input$IsletData %in% "ATAC-seq"), "Islet ATAC-seq data type is not selected")
    )
    valid_data()
    if (any(input$IsletData %in% "ATAC-seq")) {
      if (is.null(e)) {
        emin <- input$chromstart
        emax <- input$chromend
      } else {
        emin <- round(e$xmin, digits = 0)
        emax <- round(e$xmax, digits = 0)
      }
      gr_high <- GRanges(seqnames = input$chrom, IRanges(start = emin, end = emax))
      gr_atac <- GRanges(seqnames = islet_peaks$Chr, IRanges(start = islet_peaks$Start, end = islet_peaks$End))
      elementMetadata(gr_atac) <- islet_peaks[, 4:ncol(islet_peaks)]
      # find overlaps
      gr_ov <- findOverlaps(gr_atac, gr_high)
      # get data frame of intersect
      outdf <- as.data.frame(gr_atac[queryHits(gr_ov)])
      outdf_ext <- outdf[, c(1:4, 6:14)]
      colnames(outdf_ext)[1] <- "Chromosome"
      # change back width to 0 based index for BED
      outdf_ext[,4] <- as.numeric(outdf_ext[,4]) - 1
      return(outdf_ext)
    }
  }
  
  # islet atac-seq tab 
  output$isletatac <- DT::renderDataTable({
    islet_pks <- xy_range_islet_peaks(input$plot_brush)
    DT::datatable(islet_pks, escape = FALSE)
  })
  
  # selected genes tab
  xy_range_genes <- function(e) {
    shiny::validate(
      need(any(input$EndoCData %in% "Genes"), "Genes data type is not selected")
    )
    valid_data()
    if (any(input$EndoCData %in% "Genes")) {
      if (is.null(e)) {
        emin <- input$chromstart
        emax <- input$chromend
      } else {
        emin <- round(e$xmin, digits = 0)
        emax <- round(e$xmax, digits = 0)
      }
      gr_high <- GRanges(seqnames = input$chrom, IRanges(start = emin, end = emax))
      gr_atac <- GRanges(seqnames = gtf.res$V1, IRanges(start = gtf.res$V4, end = gtf.res$V5), strand = gtf.res$V7)
      elementMetadata(gr_atac) <- gtf.res[, c(4, 6:8)]
      # find overlaps
      gr_ov <- findOverlaps(gr_atac, gr_high)
      # get data frame of intersect
      outdf <- as.data.frame(gr_atac[queryHits(gr_ov)])
      outdf_ext <- outdf[, c(1:6, 8:9)]
      colnames(outdf_ext)[c(1, 6:8)] <- c("Chromosome", "Gene Symbol", "Expressed in EndoC", "Expressed in Islet")
      # change back width to 0 based index for BED
      outdf_ext[,4] <- as.numeric(outdf_ext[,4]) - 1
      return(outdf_ext)
    }
  }
  
  # genes table output
  output$gens <- DT::renderDataTable({
    out_gens <- xy_range_genes(input$plot_brush)
    DT::datatable(out_gens, escape = FALSE)
  })
  
  # snp table output
  output$snptable <- DT::renderDataTable({
    snps.res <- xy_range_snps(input$plot_brush)
    DT::datatable(snps.res)
  })
  
  # table showing intersecting chromatin interaction data at specific coordinates
  output$chromatin_ints <- DT::renderDataTable({
    withProgress(expr = all_ov <- overlap_loops(chrom = as.character(input$chrom),
                                                chromstart = as.numeric(input$chromstart),
                                                chromend = as.numeric(input$chromend)),
                 message = "Finding nearby chromatin interactions, please wait")
    DT::datatable(all_ov)
  })
  
  ########################################################################################
  # guided tutorial
  ########################################################################################
  # initiate hints on startup with custom button and event
  hintjs(session, options = list("hintButtonLabel"="Close Hint"),
         events = list())
  observeEvent(input$help,
               introjs(session, options = list("nextLabel"="Next Step",
                                               "prevLabel"="Previous Step",
                                               "skipLabel"="Quit"),
                       events = list("oncomplete"=I('alert("End of Tutorial")')))
  )
  
  ######################################################################################################
  # Download figures and tables
  ######################################################################################################
  # download figure panel
  output$Download_Plot <- downloadHandler(
    filename = function() { paste("EndoC.Islet.Omics", input$chrom, input$chromstart, input$chromend, "plot.pdf", sep=".") },
    content = function(file) {
      pdf(file, onefile = T, height = 12, width = 12)
      par(mfrow=c(11,1), mar=c(2,4,2,1))
      plotout_pdf()
      dev.off()
    })
  
  # download snp table
  output$Download_SNPs <- downloadHandler(
    filename = function() {
      coords <- get_coords(input$plot_brush)
      paste("EndoC", input$chrom, coords[1], coords[2], "gwas.snps.csv", sep=".")
    },
    content = function(file) {
      snps.res <- xy_range_snps(input$plot_brush)
      write.csv(snps.res, file, row.names = FALSE)
    }
  )
  
  # download chromatin interactions
  output$Download_Nearby <- downloadHandler(
    filename = function() {
      paste("EndoC.Islet", input$chrom, input$chromstart, input$chromend, "nearby.chromatin.interactions.csv", sep=".")
    },
    content = function(file) {
      all_ov <- overlap_loops(chrom = as.character(input$chrom),
                              chromstart = as.numeric(input$chromstart),
                              chromend = as.numeric(input$chromend))
      write.csv(all_ov, file, row.names = FALSE)
    }
  )
  
  # download selected endoc chromhmm
  output$Download_EndoC_Chrom <- downloadHandler(
    filename = function() {
      coords <- get_coords(input$plot_brush)
      paste("EndoC", input$chrom, coords[1], coords[2], "chromatin.states.csv", sep=".")
    },
    content = function(file) {
      endoc_chrom <- xy_range_chromhmm_endo(input$plot_brush)
      write.csv(endoc_chrom, file, row.names = FALSE)
    }
  )
  
  # download selected islet chromhmm
  output$Download_Islet_Chrom <- downloadHandler(
    filename = function() {
      coords <- get_coords(input$plot_brush)
      paste("Islet", input$chrom, coords[1], coords[2], "chromatin.states.csv", sep=".")
    },
    content = function(file) {
      islet_chrom <- xy_range_chromhmm_islet(input$plot_brush)
      write.csv(islet_chrom, file, row.names = FALSE)
    }
  )
  
  # download selected endoc chia
  output$Download_EndoC_ChIA <- downloadHandler(
    filename = function() {
      coords <- get_coords(input$plot_brush)
      paste("EndoC", input$chrom, coords[1], coords[2], "ChIAPET.csv", sep=".")
    },
    content = function(file) {
      endoc_chia <- xy_range_chia(input$plot_brush)
      write.csv(endoc_chia, file, row.names = FALSE)
    }
  )
  
  # download selected endoc hic
  output$Download_EndoC_HIC <- downloadHandler(
    filename = function() {
      coords <- get_coords(input$plot_brush)
      paste("EndoC", input$chrom, coords[1], coords[2], "HiC.csv", sep=".")
    },
    content = function(file) {
      endoc_hic <- xy_range_endohic(input$plot_brush)
      write.csv(endoc_hic, file, row.names = FALSE)
    }
  )
  
  # download selected islet hic
  output$Download_Islet_HIC <- downloadHandler(
    filename = function() {
      coords <- get_coords(input$plot_brush)
      paste("Islet", input$chrom, coords[1], coords[2], "HiC.csv", sep=".")
    },
    content = function(file) {
      islet_hic <- xy_range_islethic(input$plot_brush)
      write.csv(islet_hic, file, row.names = FALSE)
    }
  )
  
  # download selected endoc atac-seq peaks
  output$Download_EndoC_ATAC <- downloadHandler(
    filename = function() {
      coords <- get_coords(input$plot_brush)
      paste("EndoC", input$chrom, coords[1], coords[2], "ATAC.csv", sep=".")
    },
    content = function(file) {
      endo_atac <- xy_range_endoc_peaks(input$plot_brush)
      write.csv(endo_atac, file, row.names = FALSE)
    }
  )
  
  # download selected islet atac-seq peaks
  output$Download_Islet_ATAC <- downloadHandler(
    filename = function() {
      coords <- get_coords(input$plot_brush)
      paste("Islet", input$chrom, coords[1], coords[2], "ATAC.csv", sep=".")
    },
    content = function(file) {
      islet_atac <- xy_range_islet_peaks(input$plot_brush)
      write.csv(islet_atac, file, row.names = FALSE)
    }
  )
  
  # download selected genes and expression info in table
  output$Download_Genes <- downloadHandler(
    filename = function() {
      coords <- get_coords(input$plot_brush)
      paste("Omics", input$chrom, coords[1], coords[2], "genes.csv", sep=".")
    },
    content = function(file) {
      select_genes <- xy_range_genes(input$plot_brush)
      write.csv(select_genes, file, row.names = FALSE)
    }
  )
  
  # download manual/instructions
  output$Download_Manual <- downloadHandler(
    filename = function() {
      paste("Data/EndoC_Islet_Omics_Shiny_App_Manual.docx", sep = "")
    },
    content = function(file) {
      file.copy("Data/EndoC_Islet_Omics_Shiny_App_Manual.docx", file)
    }
  )
})

# Run the app -----------------------------------------------------------------
shinyApp(ui = ui, server = server)
