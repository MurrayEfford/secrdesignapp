# app.R 2018-12-10
# renamed from secrdesignapp.R

library(secrdesign)

# requires package rgdal to read shapefiles
# requires package parallel for max cores in simulate options (distributed with base R)
# requires package tools for file path when reading shapefiles (distributed with base R)

linewidth <- 2

# Define UI 
ui <- fluidPage(
    includeCSS("secrdesignstyle.css"),
    br(),
    navlistPanel(id = "navlist", widths = c(2,10), well=TRUE,
                 
                 "secrdesign app 1.0",
                 
                 tabPanel("Design",
                          fluidRow(
                              column(3, offset = 0,
                                     h2("Detector array"),
                                     wellPanel(
                                         fluidRow(
                                             column(6, selectInput("detector", "Detector type",
                                                                   choices = c("multi","proximity","count"),
                                                                   selected = "proximity", width = 110)),
                                             column(6, uiOutput('detectorhelp'))
                                         ),
                                         tabsetPanel(
                                             type = "pills", id = "arrayinput", selected = "Grid",
                                             
                                             tabPanel("Grid",
                                                      br(),
                                                      fluidRow(
                                                          column(6, numericInput("ny",
                                                                                 "rows", # y dimension:",
                                                                                 value = 8,
                                                                                 min = 1,
                                                                                 max = 40,
                                                                                 step = 1,
                                                                                 width = 80)),
                                                          column(6, numericInput("nx",
                                                                                 "columns", # x dimension:",
                                                                                 value = 8,
                                                                                 min = 1,
                                                                                 max = 40,
                                                                                 step = 1,
                                                                                 width = 80))
                                                      ),
                                                      fluidRow(
                                                          column(6,numericInput("spy",
                                                                                "row spacing (m)",
                                                                                value = 20,
                                                                                min = 1,
                                                                                max = 10000,
                                                                                step = 1)),
                                                          column(6,numericInput("spx",
                                                                                "col spacing (m)",
                                                                                value = 20,
                                                                                min = 1,
                                                                                max = 10000,
                                                                                step = 1))
                                                      ),

                                                      fluidRow(
                                                          column(6, checkboxInput("hollow", "Hollow", FALSE))
                                                      )
                                             ),
                                             tabPanel("Line",
                                                      br(),
                                                      fluidRow(
                                                          column(6, numericInput("nline",
                                                                                 "number of detectors",
                                                                                 value = 20,
                                                                                 min = 1,
                                                                                 max = 100,
                                                                                 step = 1))

                                                      ),
                                                      fluidRow(
                                                          column(6,numericInput("spline",
                                                                                "spacing (m)",
                                                                                value = 20,
                                                                                min = 1,
                                                                                max = 10000,
                                                                                step = 1))
                                                      ),
                                                      helpText(HTML("Warning: results with linear arrays may be highly biased if home ranges are elongated and aligned"))),
                                             # tabPanel("Pgrid",
                                             #          br(),
                                             #          fluidRow(
                                             #              column(6,numericInput("sppgrid",
                                             #                                    "spacing (m)",
                                             #                                    value = 20,
                                             #                                    min = 1,
                                             #                                    max = 10000,
                                             #                                    step = 1))),
                                             #          fileInput("polyfilename", "",
                                             #                    accept = c('.shp','.dbf','.sbn','.sbx',
                                             #                               '.shx',".prj"), multiple = TRUE)
                                             #
                                             #          ),
                                             tabPanel("File", values = "File",
                                                      fileInput("trapfilename", "",   # Detector layout file
                                                                accept = "text/plain"),
                                                      helpText(HTML(paste0("Requires text file with detector ID ",
                                                                           "and x-y coordinates in three columns,",
                                                                           " as for secr::read.traps")))
                                             )
                                         )
                                     ),

                                     fluidRow(
                                         column(4, actionButton("resetbtn", "All defaults")),
                                         
                                         column(6, actionButton("suggestbtn", "Suggest spacing"))
                                     )
                              ),

                              column(3, offset = 0,
                                     
                                     h2("Parameters"),
                                     wellPanel(
                                         
                                         fluidRow(
                                             column(6, numericInput("D",
                                                      "D (animals / ha)",
                                                      min = 0,
                                                      max = 1000,
                                                      value = 5,
                                                      step=0.1,
                                                      width = 180),
                                                    uiOutput('persqkm')
                                             ),
                                         column(6, selectInput("detectfn", "Detection function",
                                                     choices = c("HHN","HEX"),
                                                     selected = "HHN", width=110))
                                         ),
                                         fluidRow(
                                             column(6, numericInput("lambda0",
                                                                    "lambda0",
                                                                    value = 0.2,
                                                                    min = 0,
                                                                    max = 100,
                                                                    step = 0.01,
                                                                    width = 180)),
                                             column(6, numericInput("sigma",
                                                                    "sigma (m)",
                                                                    min = 0,
                                                                    max = 1000,
                                                                    value = 25,
                                                                    step = 1,
                                                                    width = 180)))
                                     ),
                                     
                                     h2("General design variables"),
                                     fluidRow(
                                         column(6,
                                                wellPanel(
                                                    numericInput("noccasions",
                                                                 "Occasions",
                                                                 value = 5,
                                                                 min = 1,
                                                                 max = 100,
                                                                 step = 1,
                                                                 width = 220),
                                                    numericInput("nrepeats",
                                                                 "Clusters",
                                                                 value = 1,
                                                                 min = 1,
                                                                 max = 100,
                                                                 step = 1,
                                                                 width = 220),
                                                    helpText(HTML("Repeated arrays"))
                                                )),
                                         column(6,
                                                wellPanel(
                                                    radioButtons("distributionbtn", label = "Distribution of n",
                                                                 choices = c("Poisson", "Binomial"))
                                                )
                                         )
                                         
                                     ),
                                     
                                     fluidRow(
                                         column(5, actionButton("simulatebtn2", "Simulate",  width = 120)),
                                         column(5, actionButton("appendbtn", "Add to summary",  width = 150))
                                     )
                                     
                              ),
                              
                              column (4,
                                      h2("Results"),
                                      verbatimTextOutput("nrmPrint"),
                                      br(),
                                      tabsetPanel(
                                          tabPanel("Array",
                                                   plotOutput("gridPlot", height = 340),
                                                   fluidRow(verbatimTextOutput("ntrapPrint"))
                                      ),
                                          tabPanel("Detectfn", plotOutput("detnPlot", height = 320)),
                                          tabPanel("Popn", 
                                                   plotOutput("popPlot", height = 320),
                                                   
                                                   fluidRow(
                                                       column(4, checkboxInput("showmaskbox", "Display mask",
                                                                     value = FALSE,
                                                                     width = 180),
                                                              checkboxInput("onlymaskbox", "Restrict to mask",
                                                                            value = TRUE,
                                                                            width = 180)
                                                              ),
                                                       column(4, checkboxInput("showHRbox", "Display 95% HR",
                                                                               value = FALSE,
                                                                               width = 180),
                                                              uiOutput('uipopN')),
                                                       column(4, actionButton("randompopbtn", "Randomize"),
                                                              helpText(HTML("Pick another realisation"))
                                                              )
                                                   )
                                                   ),
                                          tabPanel("Pxy",
                                                   plotOutput("pxyPlot", height = 320, click = "pxyclick"),
                                                   helpText(HTML("p.(x) is the probability an animal at point x will be detected at least once"))
                                          ),
                                          tabPanel("Power",
                                                   plotOutput("powerPlot", height = 320),
                                                   fluidRow(
                                                       column(8,
                                                              sliderInput("RSEslider", "",
                                                                          min = 1.0,
                                                                          max = 40,
                                                                          value = 1,
                                                                          step = 0.1,
                                                                          pre = "RSE ",
                                                                          post = "%",
                                                                          width = 340)),
                                                       column(4,
                                                              br(),
                                                              # checkboxInput("penalisedRSEbox", "Use penalised RSE",
                                                              #               value = TRUE,
                                                              #               width = 130),
                                                              checkboxInput("adjustRSEbox", "Adjust final RSE",
                                                                            value = TRUE,
                                                                            width = 130),
                                                              helpText(HTML("Scales with population")))
                                                       )
                                                   ),
                                                   type = "pills",
                                                   id = "tabs"
                                          )
                                      )
                              )
                 ),

                 tabPanel("Costing",
                          fluidRow(
                              column(3,
                                     h2("Unit cost"),
                                     wellPanel(
                                         numericInput("perkm",
                                                      "Travel per km $",
                                                      value = 0,
                                                      min = 0,
                                                      max = 1000,
                                                      step = 0.1,
                                                      width = 200),
                                         numericInput("perarray",
                                                      "Cost per array $",
                                                      min = 0,
                                                      max = 10000,
                                                      value = 0,
                                                      step=0.1,
                                                      width = 200),
                                         numericInput("perdetector",
                                                      "Cost per detector $",
                                                      min = 0,
                                                      max = 1000,
                                                      value = 0,
                                                      step=0.1,
                                                      width = 200),
                                         numericInput("pervisit",
                                                      "Cost per visit $",
                                                      min = 0,
                                                      max = 1000,
                                                      value = 0,
                                                      step=0.1,
                                                      width = 200),
                                         numericInput("perdetection",
                                                      "Cost per detection $",
                                                      min = 0,
                                                      max = 1000,
                                                      value = 0,
                                                      step=0.1,
                                                      width = 200))
                              ),
                              column(3, offset = 0,
                                     h2("Route"),
                                     wellPanel(
                                         radioButtons("routetype", label = "Type",
                                                      choices = c("Sequential", "Manual", "SumSpacing")),
                                         #   choices = c("Sequential", "SumSpacing", "TSP", "Manual")),
                                         # selectInput("TSPmethod",
                                         #             "TSP method",
                                         #             c("two_opt", "farthest_insertion", "nearest_insertion", "nn"),
                                         #             "farthest_insertion",
                                         #             width = 200),
                                         # numericInput("TSPrepl",
                                         #              "TSP replicates",
                                         #              min = 1, max = 10000,
                                         #              value = 100,
                                         #              step=1,
                                         #              width=180),
                                         checkboxInput("returnbox", "Return to start",
                                                       value = FALSE,
                                                       width = 180)
                                     ),
                                     br(),
                                     plotOutput("routePlot", click = "click", height=320),
                                     conditionalPanel("input.routetype == 'Manual'",
                                            actionButton("routebtn", "Define new route by clicking at vertices",
                                                         width = 270))

                              ),
                              column(4,
                                     h2("Current costing"),
                                     verbatimTextOutput("costPrint")
                              )
                          )
                 ),
                 #################################################################################################

                 tabPanel("Simulation",
                          fluidRow(
                              column(6,
                                     h2("Standalone R code"),
                                     verbatimTextOutput("simcodePrint"),
                                     
                                     fluidRow(
                                         column(5,
                                                actionButton("simulatebtn", "Click to execute", width = 200))
                                     ),
                                     h2("Results"),
                                     verbatimTextOutput("simPrint")
                              ),
                              column(3,
                                     h2("Simulation control"),
                                     fluidRow(
                                         column(8,
                                                wellPanel(
                                                    radioButtons("packagebtn", label = "Fit simulated data",
                                                                 choices = c("openCR.fit", "secr.fit"))
                                                )
                                         )
                                         
                                     ),
                                     fluidRow(
                                         column(8,
                                                wellPanel(
                                                    
                                                    numericInput("nrepl",
                                                                 "Number of replicates",
                                                                 min = 1,
                                                                 max = 1000,
                                                                 value = 5,
                                                                 step = 1,
                                                                 width = 180),
                                                    numericInput("ncores",
                                                                 "Number of cores",
                                                                 min = 1,
                                                                 max = parallel::detectCores(),
                                                                 value = 1,
                                                                 step = 1,
                                                                 width = 180),
                                                    numericInput("seed",
                                                                 "Random seed",
                                                                 min = 0,
                                                                 max = 1e10,
                                                                 value = 0,
                                                                 step = 1,
                                                                 width = 180),
                                                    numericInput("simnx",
                                                                 "nx",
                                                                 min = 10,
                                                                 max = 1000,
                                                                 value = 32,
                                                                 step = 1,
                                                                 width = 180),
                                                    selectInput("method", "method",
                                                                choices = c("Newton-Raphson", "Nelder-Mead", "none"),
                                                                selected = "none", width=160),
                                                    checkboxInput("simappendbox", "Add to Summary", TRUE)
                                                    
                                                )
                                         )
                                     )
                              )
                          )
                 ),

                 #################################################################################################

                 tabPanel("Spacing",
                          fluidRow(
                              column(5,
                                     h2("Standalone R code"),
                                     verbatimTextOutput("spacingcodePrint"),

                                     fluidRow(
                                         column(6, 
                                                actionButton("spacingbtn", "Click to execute", width = 200)
                                         ),
                                         column(6, 
                                                checkboxInput("spacingsimbox", "with simulations", value = FALSE, width = 200),
                                                helpText(HTML('See Simulation for control options'))
                                         )
                                     ),
                                     br(),
                                     br(),
                                     h2("Results"),
                                     verbatimTextOutput("spacingPrint"),
                                     verbatimTextOutput("spacingPrint2")
                              ),
                              column(5,
                                     br(),
                                     tabsetPanel(
                                         type = "pills", id = "spacingtabs",
                                         tabPanel("RSE",
                                                  fluidRow(
                                                      column(6,
                                                             # Show a plot of the rule-of-thumb RSE
                                                             plotOutput("RSEPlot", width = "520px", height = "400px")
                                                      )
                                                  )
                                         ),
                                         tabPanel("nrm",
                                                  fluidRow(
                                                      br(),
                                                      column(1),
                                                      column(11,verbatimTextOutput("nrmlegend"))
                                                  ),
                                                  fluidRow(
                                                      column(6,
                                                             # Show a plot of the expected number of detections vs spacing
                                                             plotOutput("nrmPlot", width = "520px", height = "400px")
                                                      )
                                                  )
                                         ),
                                         tabPanel("Cost",
                                                  fluidRow(
                                                      column(6,
                                                             # Show a plot of the cost vs spacing
                                                             plotOutput("costPlot", width = "520px", height = "400px")
                                                      )
                                                  )


                                         )
                                     )
                              )
                          )
                 ),
                 #################################################################################################

                 tabPanel("Summary",
                          fluidRow(
                              column(12,
                                     # h2("Results"),
                                     div(tableOutput("summarytable"), style = "font-size:80%")
                              )
                          ),
                          fluidRow(
                              column(2, actionButton("clearallbtn", "Clear all")),
                              column(2, actionButton("clearlastbtn", "Delete last")),
                              column(2, downloadButton("downloadData", "Download"))
                          )
                 ),
                 #################################################################################################
                 
                 tabPanel("Options",

                          # fluidRow(
                          #     column(4, textInput("savefilename", "File name", "log.txt", width = 200)),
                          #     column(4, checkboxInput("appendbox", "Append", TRUE)),
                          #     column(4, actionButton("savebtn", "Save", width = 150))
                          # ),

                          fluidRow(
                              column(3,

                                     h2("Detector array"),
                                     wellPanel(
                                         checkboxInput("lockxy", "Couple row and column dimensions", TRUE)
                                     ),

                                     h2("RSE correction factor"),
                                     wellPanel(
                                         sliderInput("CFslider", "",
                                                     min = 0.8,
                                                     max = 2.0,
                                                     value = 1,
                                                     step = 0.001,
                                                     pre = "CF ",
                                                     post = "",
                                                     width = 340),
                                         checkboxInput("updateCFbox", "Update from simulations",
                                                       value = TRUE,
                                                       width = 180)
                                     ),

                                     h2("Habitat mask"),
                                     fluidRow(
                                         column(6, wellPanel(
                                             numericInput("habxsigma", "Buffer width (multiple of sigma)",
                                                          min = 0,
                                                          max = 20,
                                                          value = 4,
                                                          step = 0.5,
                                                          width = 250),
                                             numericInput("habnx", "nx",
                                                          min = 10,
                                                          max = 1000,
                                                          value = 64,
                                                          step = 1,
                                                          width = 180)
                                             )
                                         ),
                                         
                                         column(6, wellPanel(
                                             radioButtons("maskshapebtn", label = "Shape",
                                                          choices = c("Rectangular", "Rounded"), 
                                                          selected = "Rounded")
                                         )
                                         )
                                     ),
                                     
                                     checkboxInput("polygonbox", "Clip to polygon",
                                                   value = FALSE,
                                                   width = 180),
                                     fileInput("habpolyfilename", "Polygon shapefile (all parts)",
                                               accept = c('.shp','.dbf','.sbn','.sbx',
                                                          '.shx',".prj"), multiple = TRUE),
                                     helpText(HTML("(at least xxx.shp, xxx.dbf, and xxx.shx)"))
                                     
                              ),
                          

                              column(3,
                                     # h2("Population plot"),
                                     # wellPanel(
                                     # 
                                     # ),

                                     h2("Pxy contour plot"),
                                     wellPanel(
                                         numericInput("pxyborder", "Border (spacing units)",
                                                      min = 0,
                                                      max = 10,
                                                      value = 3,
                                                      step = 0.5,
                                                      width = 180),
                                         numericInput("pxynx", "Mesh dimension nx",
                                                      min = 32,
                                                      max = 512,
                                                      value = 64,
                                                      step = 32,
                                                      width = 180),
                                         checkboxInput("pxyfillbox", "Fill contours",
                                                       value = TRUE,
                                                       width = 180),
                                         checkboxInput("pxyframebox", "Show frame",
                                                       value = FALSE,
                                                       width = 180),
                                         checkboxInput("pxylabelbox", "Label contours",
                                                       value = TRUE,
                                                       width = 180)
                                     ),
                                     h2("Spacing plot"),
                                     wellPanel(
                                         "Spacing relative to sigma", br(), br(),
                                         fluidRow(
                                             column (6, numericInput("fromR",
                                                                     "Minimum",
                                                                     min = 0.01, max = 4,
                                                                     value = 0.2,
                                                                     step = 0.2,
                                                                     width = 180)),
                                             column(6,
                                                    numericInput("toR",
                                                                 "Maximum",
                                                                 min = 1, max = 10,
                                                                 value = 4,
                                                                 step = 0.2,
                                                                 width = 180))),
                                         fluidRow(
                                             column (6, 
                                                     numericInput("byR",
                                                                  "Increment",
                                                                  min = 0.02, max = 2,
                                                                  value = 0.2,
                                                                  step = 0.02,
                                                                  width = 180)),
                                             column (6, 
                                                     numericInput("simbyR",
                                                                  "Simulation increment",
                                                                  min = 0.02, max = 2,
                                                                  value = 0.4,
                                                                  step = 0.02,
                                                                  width = 180)))
                                     )
                                     
                              ),

                              column(3,
                                     h2("Power plot"),
                                     wellPanel(
                                         fluidRow(
                                             column(8, radioButtons("powerplotbtn", label = "Plot style",
                                                      choices = c("Null hypothesis power", "Confidence interval"))),
                                             column(4, br(), br(), uiOutput('CIpct'))
                                         ),
                                         fluidRow(
                                             column(6, numericInput("alpha", "alpha",
                                                                    min = 0.001,
                                                                    max = 0.200,
                                                                    value = 0.05,
                                                                    step = 0.01,
                                                                    width = 120))
                                         ),
                                         fluidRow(
                                             column(6, numericInput("minEffect", "xmin",
                                                                    min = -99,
                                                                    max = 0,
                                                                    value = -99,
                                                                    step = 1,
                                                                    width = 180)),
                                             column (6, numericInput("maxEffect", "xmax",
                                                                     min = 0,
                                                                     max = 300,
                                                                     value = 150,
                                                                     step = 1,
                                                                     width = 180))
                                         ),
                                         
                                         
                                         h3("Hypothesis test only"),
                                         fluidRow(
                                             column(6,selectInput("testtype", "Test type",
                                                         choices = c("two.sided", "decrease", "increase"),
                                                         selected = "two.sided",
                                                         width = 140)),
                                             column(6, numericInput("target", "target power %",
                                                          min = 50,
                                                          max = 99,
                                                          value = 80,
                                                          step = 1,
                                                          width = 180))
                                             )
                                     )
                              )
                          )
                 ),
                 tabPanel("Help",
                          withMathJax(includeMarkdown("help.rmd"))
                 ),
                 tabPanel("About",
                          h2("secrdesign app 1.0"), br(),
                          
                          h5(paste("This Shiny application provides an interface to the R package 'secrdesign', version", 
                                   packageDescription("secrdesign")$Version), "."),
                          br(),
                          h5("For further information see "), 
                          a("www.otago.ac.nz/density", href="https://www.otago.ac.nz/density", target="_blank"), br(),
                          a("CRAN.R-project.org/package=secrdesign", href="https://CRAN.R-project.org/package=secrdesign", target="_blank"),
                          br(), br(),
                          
                          h5("Citation"),
                          h5("[The preferred citation for this package is not finalised]"),
                          h5("Efford, M. G. 2018. Interactive design of spatially explicit capture-recapture studies. In prep.")
                 )
               
    )
    
)

############################################################################################
# Define server logic

server <- function(input, output, session) {

    desc <- packageDescription("secrdesign")
    showNotification(paste("secrdesign", desc$Version, desc$Date),
                     closeButton = FALSE, type = "message", duration = 4)

    ##############################################################################
    
    ## miscellaneous functions
    
    ## pathlength
    ## cost
    ## plotpower    
    ## plotpowerCI    
    ## addsequence
    ## n.eq.r
    
    
    ##############################################################################
    
    pathlength <- function (trps, type = c("sequential", "sumspacing", "manual"),
                            returntostart) {
        type <- match.arg(type)
        pathl <- NA
        if (type %in% c("sequential", "tsp", "manual")) {
            if (type=="manual")
                seq <- manualroute$seq
            else
                seq <- attr(trps, "seq")
            if (!is.null(seq)) {
                #     stop("requires seq attribute or manual route")
                if (length(seq)==1)
                    pathl <- 0
                else {
                    trps <- trps[seq,]
                    ntrps <- nrow(trps)
                    if (returntostart)
                        dxy <- trps - trps[c(2:ntrps, 1),]
                    else
                        dxy <- trps[-1,] - trps[-ntrps,]
                    d <- sqrt(apply(dxy^2,1,sum))
                    pathl <- sum(d)
                }
            }
        }
        else if (type == "sumspacing") {
            d <- as.matrix(dist(trps))
            diag(d) <- NA
            d <- apply(d,1,min, na.rm = TRUE)
            if (length(table(d)) == 1)
                (nrow(trps)-1+returntostart) * d[1]
            else
                NA
        }
        else {
            NA
        }
    }
    ##############################################################################

    cost <- function (x, costs = NULL) {
        if (!inherits(x, "optimalSpacing"))
            stop("requires input of class optimalSpacing")
        if (is.null(costs))
            stop("provide costs as list with components 'perkm', 'perarray', 'perdetector', 'pervisit', 'perdetection'")
        df <- x$rotRSE$values
        df$C <- apply(df[, c("n","r")],1,sum)
        trps <- attr(x, "traps")
        sig <- attr(x, "detectpar")$sigma
        nocc <- attr(x, "noccasions")
        nrep <- attr(x, "nrepeats")

        ## initial path length in sigma units
        pathl <- pathlength(trps, tolower(input$routetype), input$returnbox)
        df$pathlength <- df$R * pathl / spacing(trps) * sig / 1000

        df$pathcost  <- costs$perkm * df$pathlength * (nocc+1) * nrep
        df$arraycost <- costs$perarray * nrep
        df$detrcost  <- costs$perdetector * nrow(trps) * nrep
        df$visitcost <- costs$pervisit * nrow(grid()) * (nocc+1) * nrep
        df$detncost  <- costs$perdetection * df$C
        df$totalcost <- apply(df[,c("detrcost","arraycost","pathcost","visitcost","detncost")], 1, sum)
        df
    }
    ##############################################################################

    plotpower <- function (RSE = 0.2, effectRange = c(-99,150), testtype = "two.sided",
                           effectIncr = 2, adjustRSE = FALSE, alpha = 0.05,
                           targetpower = 80, col = topo.colors(8)[2], add = FALSE, ...) {

        power <- function (D2D1, RSE, adjustRSE, testtype) {
            if (adjustRSE) {
                sdiff <- (log(1 + RSE^2) + log(1 + RSE^2 / D2D1))^0.5
                effect <- log(D2D1) - log(sqrt(1 + RSE^2 / D2D1)) + log(sqrt(1 + RSE^2))
            }
            else {
                sdiff <- (2 * log(1 + RSE^2))^0.5
                effect <- log(D2D1)
            }
            effect <- effect / sdiff
            if (testtype == "two.sided") {
                z.alpha <- qnorm(1-alpha/2)
                pnorm(effect - z.alpha, 0, 1, lower.tail = TRUE) +
                pnorm(-effect - z.alpha, 0, 1, lower.tail = TRUE)
            }
            else if (testtype == "decrease") {
                z.alpha <- qnorm(1-alpha)
                pnorm(-effect - z.alpha, 0, 1, lower.tail = TRUE)
            }
            else {
                z.alpha <- qnorm(1-alpha)
                pnorm(effect - z.alpha, 0, 1, lower.tail = TRUE)
            }
        }

        if (!add) {
            xlim <- effectRange
            if (xlim[1] < -98) xlim[1] <- -100
            plot(0,0,type='n', xlim = xlim, ylim=c(0,100), yaxs='i', xaxs = 'i',
                 xlab = "", ylab = 'Power %', las=1)
            mtext(side=1, line=2.5, 'Population change %')
        }
        xval <- seq(effectRange[1], effectRange[2], effectIncr)
        ## get critical values
        zero <- which.min(abs(xval))
        dpower <- function (x, target = targetpower) {
            100 * power(x/100+1, RSE, adjustRSE, testtype) - target
        }
        if (100*power(xval[1]/100+1, RSE, adjustRSE, testtype) >= targetpower) {
            lower <- uniroot(dpower, interval = xval[c(1,zero)])$root
            polygon (c(-100,-100,lower, lower), c(0,100,100, 0), col = 'lightgreen')
            text (lower, 105, as.character(round(lower, 1)), cex=0.8, xpd = TRUE)
        }
        else lower <- NA
        if (100*power(xval[length(xval)]/100+1, RSE, adjustRSE, testtype) >= targetpower) {
            upper <- uniroot(dpower, interval = xval[c(zero, length(xval))])$root
            polygon (c(upper, upper, effectRange[2], effectRange[2]), c(0,100,100, 0), col = 'lightgreen')
            text (upper, 105, as.character(round(upper, 1)), cex=0.8, xpd = TRUE)
        }
        else upper <- NA

        ## text(x = (par()$usr[3]- par()$usr[1])*0.9, y = 7, testtype, cex=0.9)

        powerpct <- 100*power(xval/100+1, RSE = RSE, adjustRSE, testtype)
        lines (xval, powerpct, col = col, lwd = linewidth)
        abline(h = targetpower, lty = 2, xpd = FALSE)
        box()
        list(lower=lower, upper = upper)
    }
    ##############################################################################
    preplus <- function(x) paste0(symnum(x, c(-Inf, 0, Inf), c("", "+")), x)

    plotpowerCI <- function (RSE = seq(0.05,0.25,0.05), effectRange = c(-99,150), 
                             estimatedRange = effectRange, adjustRSE = FALSE, 
                             alpha = 0.05, effectincr = 2, col = topo.colors(8), plt = TRUE, 
                             add = FALSE, ...) {
        
        powerCI <- function (D2D1, RSE, adjustRSE, alpha) {
            ## effect D2D1 is ratio of final and initial density estimates
            ## RSE is relative standard error of initial density estimate
            ##
            ## find SD and mean of effect size on log scale
            if (adjustRSE) {
                sdiff <- (log(1 + RSE^2) + log(1 + RSE^2 / D2D1))^0.5
                effect <- log(D2D1) - log(sqrt(1 + RSE^2 / D2D1)) + log(sqrt(1 + RSE^2))
            }
            else {
                sdiff <- (2 * log(1 + RSE^2))^0.5
                effect <- log(D2D1)
            }
            ## return back-transformed Wald interval for effect on log scale
            z.alpha <- qnorm(c(alpha/2, 1-alpha/2))
            exp(effect + sdiff * z.alpha)
        }
        
        if (!add) {
            
            xlim <- effectRange
            if (xlim[1] < -98) xlim[1] <- -100
            ylim <- xlim * c(1,1.5)
            plot(0,0,type='n', xlim = xlim, ylim = ylim, yaxs='i', xaxs = 'i',
                 xlab = "", ylab = "", las=1)
            mtext (side=1, line=2.5, 'Population change %')
            mtext (side=2, line=3, 'Estimated population change %')
            abline(v=0, lty=2)
            abline(h=0, lty=2)
            abline(0,1, lty=2, col='blue')
            box()
        }
        xval <- seq(effectRange[1], effectRange[2], effectincr)
        nRSE <- length(RSE)
        ci <- array(dim=c(length(xval), 2, nRSE), dimnames = list(xval, c('lower','upper'),RSE))
        for (i in 1:nRSE) {
            ci[,,i] <- t(sapply(xval/100+1, powerCI, RSE = RSE[i], adjustRSE = adjustRSE, alpha = alpha))
            lines(xval, 100*(ci[,1,i]-1), col = col[i], ...)
            lines(xval, 100*(ci[,2,i]-1), col = col[i], ...)
        }
        
        list(RSE = RSE, effectRange = effectRange, adjustRSE = adjustRSE,
             alpha = alpha, limits = ci)
    }
    ##############################################################################

    addsequence <- function (trps, routetype) {
        if (tolower(routetype) == "sequential")
            seq <- 1:nrow(trps)
        else {
            if (tolower(routetype) == "manual")
                seq <- manualroute$seq
            else
                seq <- NA
        }
        attr(trps, "seq") <- seq
        trps
    }
    ##############################################################################
    
    n.eq.r <- function () {
        ## find minimum at n = r
        
        detectpar <- list(lambda0 = input$lambda0, sigma = input$sigma)
        old <- FALSE
        if (old) {
            msk <- mask()
            array <- grid()
            nminr <- function(R) {
                # hold traps constant, vary sigma, D, mask
                k <- input$sigma * sqrt(input$D)
                tmpsigma <- input$spx / R
                tmpD <- (k / tmpsigma)^2
                detectpar$sigma <- tmpsigma
                nrm <- Enrm(tmpD, array, msk, detectpar, input$noccasions, input$detectfn)
                nrm[1] - nrm[2]
            }
            R <- uniroot(nminr, interval = c(input$fromR, input$toR))$root
            round(R * input$sigma,1)
        }
        else {
            nminr <- function(R) {
                if (input$arrayinput == "Grid") {
                    array <- make.grid(nx = input$nx, ny = input$ny, detector = input$detector,
                                       spacex = input$spx*R, spacey = input$spy*R,
                                       hollow = input$hollow)
                }
                else if (input$arrayinput == "Line") {
                    array <- make.grid(nx = input$nline, ny = 1, detector = input$detector,
                                       spacing = input$spline*R)
                }
                else stop ("optimal spacing works only for grid or line")
                msk <- make.mask(array, buffer = input$habxsigma * input$sigma, nx = input$habnx)
                nrm <- Enrm(input$D, array, msk, detectpar, input$noccasions, input$detectfn)
                nrm[1] - nrm[2]
            }
            R <- uniroot(nminr, interval = c(input$fromR, input$toR))$root
            if (input$arrayinput == "Grid") 
                round(R * input$spx,1)
            else
                round(R * input$spline,1)
        }
    }
    ##############################################################################
    
    runsims <- function() {
        
        progress <- Progress$new(session, min=1, max=15)
        on.exit(progress$close())
        progress$set(message = 'Simulating...',
                     detail = '')
        seed <- input$seed
        if (seed == 0) seed <- NULL
        array <- grid()
        Ndist <- if (input$distributionbtn == 'Poisson') 'poisson' else 'fixed'
        fitargs = list(detectfn = input$detectfn, 
                       method = input$method, 
                       details = list(distribution= tolower(input$distributionbtn)))
        if (input$packagebtn == "openCR.fit")
            fitargs$distribution <- tolower(input$distributionbtn)
        scen <- make.scenarios(
            noccasions = input$noccasions,
            nrepeats = input$nrepeats,
            D = input$D,
            lambda0 = input$lambda0,
            sigma = input$sigma,
            detectfn = input$detectfn,
            recapfactor = 1)
        sims <- run.scenarios (
            nrepl = input$nrepl,
            scenarios = scen,
            trapset = array,
            xsigma = input$habxsigma,
            nx = input$simnx,
            fit = TRUE,
            fit.function = input$packagebtn,
            pop.args = list(Ndist = Ndist),
            fit.args = fitargs,
            ncores = input$ncores,
            byscenario = FALSE,
            seed = seed)
        output <- summary(sims)$OUTPUT[[1]]
        ## store simulation results in reactive value
        simrv$output <- list(
            proctime = sims$proctime,
            RB = output['RB','mean'] * 100,
            RBse = output['RB','se'] * 100,
            RSE = output['RSE','mean'] * 100,
            RSEse = output['RSE','se'] * 100
        )
        #}
        simrv$current <- TRUE
        if (input$updateCFbox) {
            ## CF = simulated / naive RSE value
            CF <- output['RSE','mean'] / nrm()$rotRSE
            updateSliderInput(session, "CFslider", value = CF)
        }
        if (input$simappendbox) addtosummary() 
        
    }
    
    runspacing <- function(sims = FALSE) {
        
        removeNotification("lownr")
        progress <- Progress$new(session, min=1, max=15)
        on.exit(progress$close())
        if (sims) {
            progress$set(message = 'Simulating RSE for each spacing...')
            rotrv$output <- optimalSpacing(
                D = input$D,
                traps = grid(),
                detectpar = list(lambda0 = input$lambda0, sigma = input$sigma),
                noccasions = input$noccasions,
                nrepeats = input$nrepeats,
                detectfn = input$detectfn,
                fittedmodel = NULL,
                xsigma = input$habxsigma,
                R = seq(input$fromR, input$toR, input$byR),
                CF = input$CFslider,
                distribution = tolower(input$distributionbtn),
                fit.function = input$packagebtn,
                simulationR = seq(input$fromR, input$toR, input$simbyR),
                nrepl = input$nrepl,
                plt = FALSE,
                ncores = input$ncores,
                method = input$method
            )
        }
        else {
            progress$set(message = 'Approximating RSE for each spacing...')
            rotrv$output <- optimalSpacing(
                D = input$D,
                traps = grid(),
                detectpar = list(lambda0 = input$lambda0, sigma = input$sigma),
                noccasions = input$noccasions,
                nrepeats = input$nrepeats,
                detectfn = input$detectfn,
                fittedmodel = NULL,
                xsigma = input$habxsigma,
                R = seq(input$fromR, input$toR, input$byR),
                CF = input$CFslider,
                distribution = tolower(input$distributionbtn),
                fit.function = "none",
                plt = FALSE)
        }
        rotrv$current <- TRUE
    }
    
    ##############################################################################

    ## renderUI
    
    ##############################################################################

    output$persqkm <- renderUI({
        ## display density in animals/km^2
        Dkm <- input$D * 100
        Dkmtext <- paste0(Dkm, '&nbsp; animals / km<sup>2</sup>')
        helpText(HTML(Dkmtext))
    })
    
    ##############################################################################
    output$CIpct <- renderUI({
        ## display CI as percentage
        if (input$powerplotbtn=="Confidence interval")
            helpText(HTML(paste0(round(100 *(1-input$alpha), 1), "%")))
        else 
            helpText(HTML(""))
    })
    
    ##############################################################################
    
    output$detectorhelp <- renderUI({
        helptext <- ""
        if (input$detector == 'proximity')
            helptext <- "binary proximity detector; max. one detection per animal per detector per occasion"
        else if (input$detector == 'multi')
            helptext <- "multi-catch trap; max. one detection per animal per occasion"
        else if (input$detector == 'count')
            helptext <- "count proximity detector; integer # detections per animal per occasion"
        helpText(HTML(helptext))
    })
    
    ##############################################################################
    
    output$uipopN <- renderUI({
        if (!is.null(poprv$v))
            helpText(HTML(paste0("Number in mask = ", nrow(pop()))))
        else ""
    })
    
    ##############################################################################
    
    ## reactive
    
    ## gridpathlength
    ## invalidateOutputs
    ## simarg
    ## grid
    ## mask
    ## pop
    ## Pxy
    ## nrm
    
    ##############################################################################

    gridpathlength <- reactive({
        trps <- grid()
        pathlength(trps, tolower(input$routetype), input$returnbox)
    })
    
    ##############################################################################
    
    invalidateOutputs <- reactive({
        simrv$current <- FALSE
        rotrv$current <- FALSE
        pxyrv$value <- NULL
        updateNumericInput(session, "D", step = 10^trunc(log10(input$D/50)))
    })
    
    ##############################################################################
    
    simarg <- reactive({
        simrv$current <- FALSE
        list(
            nrepl = input$nrepl,
            ncores = input$ncores,
            seed = input$seed,
            simnx = input$simnx,
            simxsigma = input$habxsigma,
            method = input$method)}
    )

    ##############################################################################

    grid <- reactive(
        {
            simrv$current <- FALSE
            rotrv$current <- FALSE
            pxyrv$value <- NULL
            trps <- NULL
            
            if (input$arrayinput == "Grid") {
                # generate array based on input$nx, ny from ui.R
                # if (input$random) {
                #     trps <- trap.builder (n = input$nx * input$ny, detector = input$detector,
                #                           region = matrix(ncol = 2,
                #                                           c(0,0,input$nx * input$spx, input$nx * input$spx,
                #                                             0, input$ny * input$spy, input$ny * input$spy, 0)))
                #
                # }
                # else
                trps <- make.grid(nx = input$nx, ny = input$ny, detector = input$detector,
                                  spacex = input$spx, spacey = input$spy,
                                  hollow = input$hollow, ID = "numxb")
            }
            else if (input$arrayinput == "Line") {
                trps <- make.grid(nx = input$nline, ny = 1, detector = input$detector,
                                  spacing = input$spline)
            }
            # else if (input$arrayinput == "Pgrid") {
            #     inFile <- input$polyfilename
            #     if (is.null(inFile))
            #         return(NULL)
            #     else {
            #         myshape <- input$polyfilename
            #         cat ('input$polyfilename \n')
            #         print(myshape)
            #         dsnname <- dirname(myshape[1,4])
            #         for ( i in 1:nrow(myshape)) {
            #             file.rename(myshape[i,4], paste0(dsnname,"/",myshape[i,1]))}
            #         filename <- list.files(dsnname, pattern="*.shp", full.names=FALSE)
            #         layername <- tools::file_path_sans_ext(basename(filename))
            #         #layername <- tools::file_path_sans_ext(basename(myshape$name))
            #
            #         if (is.null(filename))
            #             stop("provide valid filename")
            #         cat ('dsnname ', dsnname, '\n')
            #         cat ('layername ', layername, '\n')
            #         poly <- rgdal::readOGR(dsn = dsnname, layer = layername)
            #
            #         # datasource <- paste0(myshape$datapath, '/', myshape$name)
            #         # cat ('datasource ', datasource, '\n')
            #         # poly <- rgdal::readOGR(datasource)
            #         # poly <- rgdal::readOGR('c:/density secr 3.1/package data/ovpossum/ovforest.shp')
            #
            #         if ((rgeos::gArea(poly) / input$sppgrid^2) > 1000) {
            #             showNotification("> 1000 detectors; try again",
            #                              type = "warning", id = "biggrid", duration = 5)
            #             return(NULL)
            #         }
            #         else removeNotification("biggrid")
            #         trps <- make.systematic(region = poly, spacing = input$sppgrid, detector = input$detector)
            #     }
            # }
            else {
                inFile <- input$trapfilename
                if (!is.null(inFile)) {
                    filename <- input$trapfilename[1,"datapath"]
                    if (is.null(filename))
                        stop("provide valid filename")
                    trps <- read.traps (filename, detector = input$detector)
                    if (!inherits(trps, "traps")) stop ("invalid trap file")
                }
            }
            
            if (is.null(trps)) {
                hideTab(inputId = "navlist", target = "Costing")
                hideTab(inputId = "navlist", target = "Spacing")
                hideTab(inputId = "navlist", target = "Simulation")
                return (NULL)
            }
            else {
                showTab(inputId = "navlist", target = "Costing")
                showTab(inputId = "navlist", target = "Spacing")
                showTab(inputId = "navlist", target = "Simulation")
                addsequence (trps, input$routetype)
            }
        }
    )
    ##############################################################################

    poly <- reactive( {
        if (input$polygonbox) {
            inFile <- input$habpolyfilename
            if (is.null(inFile))
                poly <- NULL
            else {
                myshape <- input$habpolyfilename
                dsnname <- dirname(myshape[1,4])
                for ( i in 1:nrow(myshape)) {
                    file.rename(myshape[i,4], paste0(dsnname,"/",myshape[i,1]))}
                filename <- list.files(dsnname, pattern="*.shp", full.names=FALSE)
                layername <- tools::file_path_sans_ext(basename(filename))
                
                if (is.null(filename))
                    stop("provide valid filenames")
                if (!requireNamespace("rgdal"))
                    stop("need package rgdal to read shapefiles")
                poly <- rgdal::readOGR(dsn = dsnname, layer = layername)
                
                if (sum (pointsInPolygon(grid(), poly)) == 0) {
                    showNotification("No detectors in polygon - ignoring Clip to polygon",
                                     type = "warning", id = "noclip", duration = 5)
                    poly <- NULL
                }
                
            }
        }
        else poly <- NULL
        return(poly)
    })
    
    mask <- reactive( {
        rotrv$current <- FALSE
        pxyrv$value <- NULL
        make.mask (grid(),
                   buffer = input$habxsigma * input$sigma,
                   nx = input$habnx,
                   type = if (input$maskshapebtn=='Rectangular') 'traprect' else 'trapbuffer',
                   poly = poly(),
                   keep.poly = FALSE)
    }
    )
    ##############################################################################
    
    pop <- reactive(
        {
            poprv$v
            core <- grid()
            if (is.null(core)) return (NULL)
            if (input$D * maskarea(mask()) > 10000) {
                showNotification("population exceeds 10000; try again",
                                 type = "error", id = "bigpop", duration = 10)
                return(NULL)
            }
            else removeNotification("bigpop")
            Ndist <- if (input$distributionbtn == 'Poisson') 'poisson' else 'fixed'
            if (input$onlymaskbox)
                pop <- sim.popn (D = input$D, core=mask(), model2D="IHP", Ndist = Ndist)
            else # rectangular area
                pop <- sim.popn (D = input$D, core=core, Ndist = Ndist,
                                 buffer = input$habxsigma * input$sigma)
            pop
        }
    )
    ##############################################################################
    
    Pxy <- reactive({
        # DOES NOT USE poly()
        invalidateOutputs()
        trps <- grid()
        msk <- make.mask(trps, buffer = input$sigma * input$pxyborder, nx = input$pxynx)
        Pxy <- pdot(msk, trps, detectfn = input$detectfn,
                    detectpar=list(lambda0=input$lambda0, sigma = input$sigma),
                    noccasions = input$noccasions)
        sumPxy <- sum(Pxy)
        EPxy <- sum(Pxy^2) / sumPxy
        EPxy2 <- sum(Pxy^3) /sumPxy
        varPxy <- EPxy2 - EPxy^2
        sinuosity <- max(dist(trps)) / (spacing(trps) * (nrow(trps)-1))
        list(CVPxy = sqrt(varPxy)/EPxy, sinuosity = sinuosity, esa = sumPxy * attr(msk, "area"))
    })
    ##############################################################################

    nrm <- reactive({
        
        trps <- grid()
        if (is.null(trps)) return (NULL)
        invalidateOutputs()
        msk <- mask()
        scen <- make.scenarios(trapsindex = 1, noccasions = input$noccasions, nrepeats = input$nrepeats,
                               D = input$D, sigma = input$sigma, lambda0 = input$lambda0, detectfn = input$detectfn)
        scensum <- scenarioSummary(scen,
                                   trapset = trps,
                                   mask = msk,
                                   CF = input$CFslider,  
                                   routelength = gridpathlength() / 1000,
                                   costing = TRUE,
                                   unitcost = list(perkm = input$perkm,
                                                   perarray = input$perarray,
                                                   perdetector = input$perdetector,
                                                   pervisit = input$pervisit,
                                                   perdetection = input$perdetection))
        scensum$maskarea <- maskarea(msk)
        if (input$distributionbtn == "Binomial") {
            scensum$rotRSE <- scensum$rotRSEB     
        }
        
        RSE <- 100*scensum$rotRSE
        if (is.na(RSE))
            maxRSE <- 100
        else {
            if (RSE<10) maxRSE <- 20
            else if (RSE<20) maxRSE <- 30
            else if (RSE<30) maxRSE <- 40
            else if (RSE<40) maxRSE <- 50
            else maxRSE <- 100
        }
        updateSliderInput(session, "RSEslider",
                          min = 1.0,
                          max = maxRSE,
                          value = RSE,
                          step = 0.1)
        
        # nr
        scensum
    })
    ##############################################################################

    addtosummary <- function() {
        
        nrmval <- nrm()
        invalidateOutputs()
        
        df <- data.frame(
            date = format(Sys.time(), "%Y-%m-%d"),
            time = format(Sys.time(), "%H:%M:%S"),
            detector = input$detector,
            source = input$arrayinput,
            nx = if (input$arrayinput=="Grid") input$nx else
                if (input$arrayinput=="Line") input$nline else NA,
            ny = if (input$arrayinput=="Grid") input$ny else NA,
            spacex = if (input$arrayinput=="Grid") input$spx else
                if (input$arrayinput=="Line") input$spline else NA,
            spacey = if (input$arrayinput=="Grid") input$spy else NA,
            ndetect = nrow(grid()) * input$nrepeats,
            nocc = input$noccasions,
            nrepeats = input$nrepeats,
            distrib = input$distributionbtn,
            detectfn = input$detectfn,
            
            D = input$D,
            lambda0 = input$lambda0,
            sigma = input$sigma,
            k = round(input$D^0.5 * input$sigma / 100,3),
            
            n = round(nrmval$En,1),
            r = round(nrmval$Er,1),
            m = round(nrmval$Em,1),
            perHR = nrmval$detperHR,
            rotRSE = round(nrmval$rotRSE * 100, 1),
            CF = round(nrmval$CF, 3),
            # perkm = input$perkm,
            # perarry = input$perarray,
            # perdetr = input$perdetector,
            # perdetn = input$perdetection,
            
            route = round(gridpathlength()/1000 * input$nrepeats,3),
            cost = round(nrmval$totalcost, 2)
        )
        
        if (!simrv$current) {
            simdf <- data.frame(
                simfn = "",
                nrepl = NA,
                simtime = NA,
                simRSE = NA, # simul()$RSE,
                simRSEse = NA,
                simRB = NA,
                simRBse = NA)
        }
        else {
            simdf <- data.frame(
                simfn = input$packagebtn,
                nrepl = input$nrepl,
                simtime = round(simrv$output$proctime,2),
                simRSE = simrv$output$RSE,
                simRSEse = simrv$output$RSEse,
                simRB = simrv$output$RB,
                simRBse = simrv$output$RBse
            )
        }
        
        df <- cbind(df, simdf)
        
        sumrv$value <- rbind (sumrv$value, df)
    }
    ##############################################################################
    
    ## reactiveValues
    
    ## simrv, rotrv, RSErv, pxyrv : logical
    ## sumrv : summary table
    ## poprv
    ## manualroute
    
    ##############################################################################

    simrv <- reactiveValues(current = FALSE, output = NULL)
    rotrv <- reactiveValues(current = FALSE, output = NULL)
    RSErv <- reactiveValues(current = FALSE, value = NULL, adjRSE = NULL)
    pxyrv <- reactiveValues(current = FALSE, xy = NULL, value = NULL)
    poprv <- reactiveValues( v = 0)  # used to invalidate and re-plot popn
    manualroute <- reactiveValues(seq = NULL)
    
    sumrv <- reactiveValues(
        value = read.csv(text = paste
        ("date, time, detector, source, nx, ny, spacex, spacey, ndetect, nocc, ",
            "ncluster, distrib, detectfn, D, lambda0, sigma, k, En, Er, Em, perHR, ",
            "rotRSE, CF, route, cost, simfn,  nrepl, simtime, simRSE, simRSEse,",
            "simRB, simRBse"))
    )
    
    ##############################################################################
    
    ## observe
    
    ##############################################################################
    
    observe({
        if (input$lockxy) {
            updateNumericInput(session, "spx", value = input$spy)
            updateNumericInput(session, "nx", value = input$ny)
        }
    })
    ##############################################################################

    ## Modal dialogue to confirm simulation if it might take a long time
    
    OKModal <- function(time) {
        modalDialog(
            paste("Simulations predicted to take ", round(time,1), " minutes"),
            size = "s",
            easyClose = TRUE,
            footer = tagList(
                modalButton("Cancel"),
                actionButton("okbtn", "Continue")
            )
        )
    }
    
    ##############################################################################
    
    ## Modal dialogue to confirm spacing simulations if they might take a long time

    spacingOKModal <- function(time) {
        modalDialog(
            paste("Simulations predicted to take ", round(time,1), " minutes"),
            size = "s",
            easyClose = TRUE,
            footer = tagList(
                modalButton("Cancel"),
                actionButton("spacingokbtn", "Continue")
            )
        )
    }
    
    ##############################################################################
    
    
    ## observeEvent
    
    # suggestbtn
    # resetbtn
    # CFslider
    # spacingbtn
    # simulatebtn, simulatebtn2
    # okbtn
    # randompopbtn
    # routebtn
    # clearallbtn
    # clearlastbtn
    # hollow
    # arrayinput
    # click
    # pxyclick
    # appendbtn
    # summarybtn
    
    ##############################################################################

    observeEvent(input$suggestbtn, {
        ## E[n] == E[r]
        if (input$arrayinput %in% c("Grid", "Line"))
            optimalspacing <- n.eq.r()
        if (input$arrayinput == "Grid") {
            updateNumericInput(session, "spy", value = optimalspacing)
            updateNumericInput(session, "spx", value = optimalspacing)
        }
        else if (input$arrayinput == "Line") {
            updateNumericInput(session, "spline", value = optimalspacing)
        }
    })

    ##############################################################################
    
    observeEvent(input$resetbtn, {

        ## array
        updateSelectInput(session, "detector", selected = "proximity")
        updateTabsetPanel(session, "arrayinput", selected = "Grid")
        updateNumericInput(session, "ny", value = 8)
        updateNumericInput(session, "nx", value = 8)
        updateNumericInput(session, "nline", value = 20)
        updateNumericInput(session, "spy", value = 20)
        updateNumericInput(session, "spx", value = 20)
        updateNumericInput(session, "spline", value = 20)
        updateCheckboxInput(session, "hollow", value = FALSE )

        ## parameters
        updateNumericInput(session, "D", value = 5)
        updateSelectInput(session, "detectfn", selected = "HHN")
        updateNumericInput(session, "lambda0", value = 0.2)
        updateNumericInput(session, "sigma", value = 25)

        ## general
        updateNumericInput(session, "noccasions", value = 5)
        updateNumericInput(session, "nrepeats", value = 1)
        updateTabsetPanel(session, "tabs", selected = "Array")
        updateRadioButtons(session, "distributionbtn", "Distribution", selected = "Poisson")

        ## pop plot
        updateCheckboxInput(session, "showHRbox", "Display 95% home range", value = FALSE)
        updateCheckboxInput(session, "showmaskbox", "Display mask", value = FALSE)
        updateCheckboxInput(session, "onlymaskbox", "Restrict to mask", value = TRUE)
        
        ## power plot
        updateCheckboxInput(session, "adjustRSEbox", "Adjust final RSE", value = TRUE)

        ## costing
        updateNumericInput(session, "perkm", value = 0)
        updateNumericInput(session, "perarray", value = 0)
        updateNumericInput(session, "perdetector", value = 0)
        updateNumericInput(session, "perdetection", value = 0)
        updateNumericInput(session, "pervisit", value = 0)
        updateRadioButtons(session, "routetype", selected = "Sequential")
        updateCheckboxInput(session, "returnbox", value = FALSE)

        ## spacing
        updateTabsetPanel(session, "spacingtabs", selected = "RSE")
        updateCheckboxInput(session, "spacingsimbox", value = FALSE)

        ## simulate
        updateNumericInput(session, "nrepl", value = 5)
        updateNumericInput(session, "ncores", value = 1)
        updateNumericInput(session, "seed", value = 0)
        updateNumericInput(session, "simnx", value = 32)
        updateSelectInput(session, "method", "method", selected = "none")
        updateRadioButtons(session, "packagebtn", "Fit simulated data", selected = "openCR.fit")
        updateCheckboxInput(session, "simappendbox", value = TRUE)
        
        ## options
        # updateTextInput(session, "savefilename", value = "log.txt")
        # updateCheckboxInput(session, "appendbox", value = TRUE)
        updateCheckboxInput(session, "lockxy", value = TRUE)

        ## habitat        
        updateNumericInput(session, "habxsigma", value = 4)
        updateNumericInput(session, "habnx", value = 64)
        updateRadioButtons(session, "maskshapebtn", selected = "Rounded")
        updateCheckboxInput(session, "polygonbox", "Clip to polygon", value = FALSE)
        
        
        updateNumericInput(session, "pxyborder", value = 3)
        updateNumericInput(session, "pxynx", value = 64)
        updateCheckboxInput(session, "pxyfillbox", value = TRUE)
        updateCheckboxInput(session, "pxyframebox", value = FALSE)
        updateCheckboxInput(session, "pxylabelbox", value = TRUE)
        
        updateSliderInput(session, "CFslider", value = 1.0)
        updateCheckboxInput(session, "updateCFbox", value = TRUE)
        
        updateRadioButtons(session, "powerplotbtn", selected = "Null hypothesis power")
        updateNumericInput(session, "alpha", value = 0.05)
        updateNumericInput(session, "target", value = 80)
        updateSelectInput(session, "testtype", selected = "two.sided")
        updateNumericInput(session, "minEffect", value = -99)
        updateNumericInput(session, "maxEffect", value = 150)
        updateNumericInput(session, "fromR", value = 0.2)
        updateNumericInput(session, "toR", value = 4)
        updateNumericInput(session, "byR", value = 0.2)
        updateNumericInput(session, "simbyR", value = 0.4)

    })

    ##############################################################################

    observeEvent(input$CFslider, {
        rotrv$current <- FALSE
    })
    ##############################################################################
    
    observeEvent(input$spacingbtn, {
        ## inp <- oS2()
        
        if (input$spacingsimbox) {
            
            ## time check 2018-12-05
            methodfactor <- 1 + ((input$method != "none") * 4)
            functionfactor <- 1 + ((input$packagebtn != "openCR.fit") * 3)
            detectorfactor <- switch(input$detector, proximity = 1, multi = 0.6, count = 4)
            time <- nrow(mask()) * nrow(grid()) / 1e9 * input$nrepeats * 
                nrm()$En * input$noccasions * input$nrepl * 
                length(seq(input$fromR, input$toR, input$simbyR)) *
                methodfactor * functionfactor * detectorfactor
            
            if (time > 0.2) {
                showModal(spacingOKModal(time))
            }
            else {
                runspacing(sims = TRUE)
            }
            
        }
        else {
            runspacing(sims = FALSE)
        }
    }
    )
    ##############################################################################

    observeEvent(c(input$simulatebtn, input$simulatebtn2), {
        
        ## use test to block initial execution when simulatebtn2 goes from NULL to 0
        if ((input$simulatebtn + input$simulatebtn2>0) & !is.null(grid())) {
            removeNotification("lownr")
            methodfactor <- 1 + ((input$method != "none") * 4)
            functionfactor <- 1 + ((input$packagebtn != "openCR.fit") * 3)
            detectorfactor <- switch(input$detector, proximity = 1, multi = 0.6, count = 4)
            time <- nrow(mask()) * nrow(grid()) / 4.5e9 * input$nrepeats * 
                nrm()$En * input$noccasions * input$nrepl * 
                methodfactor * functionfactor * detectorfactor
            if (time > 0.2)
                showModal(OKModal(time))
            else {
                runsims()
            }
        }
    })
    
    ##############################################################################
    
    observeEvent(input$okbtn, {
        ## user happy: proceed with long simulation
        removeModal()
        runsims()
    })
    
    ##############################################################################
    
    observeEvent(input$spacingokbtn, {
        ## user happy: proceed with spacings and long simulation 
        removeModal()
        runspacing(sims = TRUE)   ## 2018-12-07 included sims = TRUE
    })
    
    ##############################################################################
    
    observeEvent(input$randompopbtn, {
        ## use test to block initial execution when randompopbtn goes from NULL to 0
        poprv$v <- poprv$v + 1
        
    })
    
    ##############################################################################
    
    observeEvent(input$routebtn, {
        manualroute$seq <- NULL
    }   )
    
    ##############################################################################
    
    observeEvent(input$clearallbtn, {
        sumrv$value <- sumrv$value[0,]
    }   )
    
    ##############################################################################
    
    observeEvent(input$clearlastbtn, {
        if (nrow(sumrv$value)>0)
            sumrv$value <- sumrv$value[-nrow(sumrv$value),]
    }   )
    
    ##############################################################################
    
    observeEvent(input$hollow, {
        if (input$hollow) CF <- 1.2 else CF <- 1.0
        updateSliderInput(session, "CFslider", value = CF)
    })
    
    ##############################################################################
    
    observeEvent(input$arrayinput, {
        if (input$arrayinput=="Line") CF <- 1.3
        else if (input$arrayinput=="Grid" & input$hollow) CF <- 1.2
        else CF <- 1.0
        updateSliderInput(session, "CFslider", value = CF)
    })
    
    ##############################################################################
    
    observeEvent(input$click, {
        xy <- c(input$click$x, input$click$y)
        trp <- nearesttrap(xy, grid())
        manualroute$seq <- c(manualroute$seq, trp)
    })
    
    ##############################################################################
    
    observeEvent(input$pxyclick, {
        invalidateOutputs()
        trps <- grid()
        border <- input$pxyborder * spacing(trps)
        
        xy <- c(input$pxyclick$x, input$pxyclick$y)
        if ((xy[2] < (min(trps$y) - border)) ||
            (xy[2] > (max(trps$y) + border)) ||
            (xy[1] < (min(trps$x) - border)) ||
            (xy[1] > (max(trps$x) + border)) ) {
            pxyrv$value <- NULL
        }
        else {
            Pxy <- pdot (xy, trps,
                         detectfn = input$detectfn,
                         detectpar = list(lambda0 = input$lambda0, sigma = input$sigma),
                         noccasions = input$noccasions)
            pxyrv$xy <-xy
            pxyrv$value <- Pxy}
    })
    
    ##############################################################################
    
    observeEvent(input$appendbtn, {
        if (!is.null(grid()))
            addtosummary()
    })

    ##############################################################################
    
    observeEvent(input$summarybtn, {
        ap <- isolate(input$appendbox)
        ex <- file.exists(isolate(input$savefilename))
        write.table(sumrv$value,
                    append = ap & ex,
                    file = isolate(input$savefilename),
                    col.names = !ap | !ex,
                    row.names = FALSE,
                    quote = FALSE)
    }
    )
    
    
    ## renderText
    
    # spacingcodePrint
    # nrmlegend   (Spacing nrm plot)
    # simcodePrint
    # nrmPrint 
    # costPrint
    # spacingPrint 
    # spacingPrint2
    # simPrint 

    ##############################################################################

    output$spacingcodePrint <- renderText ({
        if (is.null(grid())) "" # abort if no valid array
        else {
            
            ## inp <- oS2()
            invalidateOutputs()
            
            if (input$arrayinput == "Grid") {
                arraycode <- paste0(
                    "array <- make.grid(",
                    "nx = ", input$nx, ", ",
                    "ny = ", input$ny, ", ",
                    "spacex = ", input$spx, ", ",
                    "spacey = ", input$spy, ",\n    ",
                    "detector = '", input$detector, "', ",
                    "hollow = ", input$hollow, ")\n\n"
                )
            }
            else if (input$arrayinput == "Line") {
                arraycode <- paste0(
                    "array <- make.grid(",
                    "nx = ", input$nline, ", ",
                    "ny = 1, ",
                    "spacing = ", input$spline, ", \n    ",
                    "detector = '", input$detector, "')\n\n"
                )
            }
            else { # input$arrayinput == "File"
                
                arraycode <- paste0(
                    "array <- read.traps ('",
                    input$trapfilename[1,"name"],
                    "', detector = '", input$detector, "')\n\n"
                )
            }
            
            if (input$spacingsimbox) {
                simulationargs <- paste0(",\n    fit.function = '", input$packagebtn, "',\n",
                                         "    simulationR = seq(", input$fromR, ", ", input$toR, ", ", input$simbyR, "),\n",
                                         "    nrepl = ", input$nrepl, ", ",
                                         "ncores = ", input$ncores, ",\n",
                                         "    method = '", input$method, "', ",
                                         "seed = ", if (input$seed==0) "NULL" else input$seed
                )
            }
            else
                simulationargs <- ""
            
            
            paste0(
                "library(secrdesign)\n\n",
                
                arraycode,
                
                "oS <- optimalSpacing(D = ", input$D, ", traps = array,\n",
                "    detectpar = list(lambda0 = ", input$lambda0, ", sigma = ", input$sigma, "),\n",
                "    detectfn = '", input$detectfn, "', noccasions = ", input$noccasions, ", nrepeats = ", input$nrepeats, ",\n",
                "    R = seq(", input$fromR, ", ", input$toR, ", ", input$byR, "), CF = ", round(input$CFslider,3), ", ",
                "distribution = '", tolower(input$distributionbtn), "'",  
                simulationargs,
                ")\n\n",
                
                "plot(oS)\n"
                
            )
        }
    })
    ##############################################################################
    
    output$nrmlegend <- renderText ({
        paste0("Expected number of individuals n\n",
               "Expected number of recaptures r\n",
               "Expected number of movement recaptures m\n")
    })
    
    ##############################################################################
    
    output$simcodePrint <- renderText ({
        if (is.null(grid())) ""  # abort if no valid array
        else {
            if (input$arrayinput == "Grid") {
                arraycode <- paste0(
                    "array <- make.grid(",
                    "nx = ", input$nx, ", ",
                    "ny = ", input$ny, ", ",
                    "spacex = ", input$spx, ", ",
                    "spacey = ", input$spy, ",\n    ",
                    "detector = '", input$detector, "', ",
                    "hollow = ", input$hollow, ")\n"
                )
            }
            else if (input$arrayinput == "Line") {
                arraycode <- paste0(
                    "array <- make.grid(",
                    "nx = ", input$nline, ", ",
                    "ny = 1, ",
                    "spacing = ", input$spline, ", \n    ",
                    "detector = '", input$detector, "')\n"
                )
            }
            else { # input$arrayinput=="File"
                arraycode <- paste0(
                    "array <- read.traps ('",
                    input$trapfilename[1,"name"],
                    "', detector = '", input$detector, "')\n"
                )
            }
            
            D  <- input$D * input$nrepeats
            Ndist <- if (input$distributionbtn == 'Poisson') 'poisson' else 'fixed'
            distncode <- if (input$packagebtn == "secr.fit")
                paste0("details = list(distribution = '", tolower(input$distributionbtn), "')")
            else
                paste0("distribution = '", tolower(input$distributionbtn), "'")
            
            RBcode <- if (input$method == "none") "" else
                paste0( 
                    "  RB = output['RB','mean'] * 100,\n",
                    "  RBse = output['RB','se'] * 100,\n"
                )
            paste0(
                "library(secrdesign)\n\n",
                
                "scen <- make.scenarios(",
                "noccasions = ", input$noccasions, ", ",
                "nrepeats = ", input$nrepeats, ",\n    ",
                "D = ", input$D, ", ",
                "sigma = ", input$sigma, ", ",
                "lambda0 = ", input$lambda0, ", ",
                "detectfn = '", input$detectfn, "')\n\n",
                
                arraycode, "\n",
                
                "sims <- run.scenarios (",
                "nrepl = ", input$nrepl, ", ",
                "scenarios = scen, \n",
                "    trapset = array, ",
                ## "xsigma = ", input$simxsigma, ", ",
                "xsigma = ", input$habxsigma, ", ",
                "nx = ", input$simnx, ",\n",
                "    pop.args = list(Ndist = '", Ndist, "'),\n",
                
                "    fit = TRUE, ",
                "fit.function = '", input$packagebtn, "',\n",
                "    fit.args = list(detectfn = '", input$detectfn, "', ",
                "method = '", input$method, "',\n",
                "        ", distncode, "),\n", 
                "    ncores = ", input$ncores, ", ",
                "byscenario = FALSE, seed = ", if (input$seed==0) "NULL" else input$seed, 
                ")\n\n",
                
                "# return selected simulation results as vector\n",
                "output <- summary(sims)$OUTPUT[[1]]\n",
                "c(sims$proctime,\n",
                RBcode,
                "  RSE = output['RSE','mean'] * 100,\n",
                "  RSEse = output['RSE','se'] * 100\n)"
            )                
        }
    })
    ##############################################################################
    
    output$ntrapPrint <- renderText({
        gr <- grid()
        if (!is.null(gr)) {
            gridlength <- max(dist(gr))
            ratio <- round(gridlength/input$sigma,1)
            paste0(nrow(gr), " ", input$detector, " detectors; diameter ", round(gridlength,1), " m (",
                   ratio, " sigma)")
        }
        else ""
    })
    ##############################################################################
    
    output$nrmPrint <- renderText({
        nrmval <- nrm()
        if (is.null(nrmval)) return (NULL)
        star <- if (nrmval$nrepeats>1) "*" else ""
        nrepeatstr <- if (nrmval$nrepeats>1) paste0("\n* ", nrmval$nrepeats, " arrays") else ""
        Pxyval <- Pxy()
        
        coststr <- if (is.null(nrmval$totalcost) | (nrmval$totalcost<=0))
            ""
        else
            paste0( "\nTotal cost = $", sprintf("%.2f", nrmval$totalcost), star)
        
        if (nrmval$En<20 | nrmval$Er<20) {
            showNotification("Low E(n) or E(r) - simulate to check RSE",
                             closeButton = TRUE, type = "warning", id = "lownr", duration = NULL)
        }
        else
            removeNotification("lownr")
        paste0(
            
            "Expected number of individuals detected n = ", 
            round(nrmval$En,1), star,'\n',
            "Expected number of recaptures r = ",
            round(nrmval$Er,1), star, '\n',
            "Expected number of movement recaptures m = ",
            round(nrmval$Em,1), star,'\n',
            "Median detectors per 95% home range = ", 
            nrmval$detperHR, '\n',
            "Overlap coefficient k = ",
            round(input$D^0.5 * input$sigma / 100,3), '\n',
            
            "Effective sampling area = ",
            round(nrmval$esa,2), ' ha', star, " (mask ", round(nrmval$maskarea,2), " ha)\n",
            "Rule-of-thumb RSE = ",
            round(nrmval$rotRSE * 100, 1), "%", star, " (correction factor ", round(input$CFslider,3), ")",
            ## "\nRule-of-thumb RSE (binomial) = ",
            ## round(nrmval$rotRSEbin * 100, 1), "% (correction factor ", round(input$CFslider,3), ")",
            coststr, nrepeatstr
        )
    })
    ##############################################################################
    
    output$costPrint <- renderText({
        if (!is.null(grid())) {
            costs <- nrm()
            nocc1 <- input$noccasions + 1
            paste0(
                "  Travel     = $",
                round(costs$travel,2), "  (", round(gridpathlength()/1000  * nocc1 * input$nrepeats,3), ' km)\n',
                "  Arrays     = $",
                round(costs$arrays,2), "  (", input$nrepeats, ") \n",
                "  Detectors  = $",
                round(costs$detectors,2), "  (", nrow(grid()) * input$nrepeats, ") \n",
                "  Visits     = $",
                round(costs$visits,2), "  (", nrow(grid()) * nocc1 * input$nrepeats , ") \n",
                "  Detections = $",
                round(costs$detections,2), "  (", round(costs$En+costs$Er,1), ')\n\n',
                "  Total      = $",
                round(costs$totalcost, 2)
            )
        }
        else ""
    })
    ##############################################################################
    
    output$spacingPrint <- renderText({
        if (rotrv$current) {
            temp <- rotrv$output
            paste0("Optimal spacing (relative to sigma) = ",
                   round(temp$rotRSE$optimum.R,2), '\n',
                   "Optimal spacing (absolute) = ",
                   round(temp$rotRSE$optimum.spacing, 1), " m", '\n',
                   "Minimum rule-of-thumb RSE = ",
                   round(temp$rotRSE$minimum.RSE*100, 1), " %  (correction factor ", round(input$CFslider,3), ")")
        }
        else NULL
    })
    ##############################################################################
    
    output$spacingPrint2 <- renderText(
        if (rotrv$current) {
            if (is.null(rotrv$output$simRSE)) {
                NULL
            }
            else {
                temp <- rotrv$output$simRSE$summary
                temp <- round(temp,4)
                paste(capture.output(temp), collapse = "\n")
            }
        }
        else NULL
    )
    ##############################################################################
    
    output$simPrint <- renderText({
        tmp <- simarg()
        if (simrv$current) {
            sims <- simrv$output
            out <- paste0(
                "Number of replicates = ",
                tmp$nrepl, "\n",
                "Time for simulations = ",
                round(sims$proctime,2), " secs",  "\n",
                "Simulated RSE = ",
                round(sims$RSE, 2), "%", " (SE ",  round(sims$RSEse, 2), "%)", "\n")
            if (input$method != "none") {
                out <- paste0(out,
                              "Simulated RB = ",
                              round(sims$RB, 2), "%", " (SE ",  round(sims$RBse, 2), "%)")
            }
            out
        }
        else
            NULL
    })
    ##############################################################################
    
    ## renderPlot
    
    ## gridPlot
    ## routePlot
    ## detnPlot
    ## popPlot
    ## pxyPlot
    ## powerPlot
    ## RSEPlot
    ## nrmPlot
    ## costPlot
    
    ##############################################################################
    
    output$gridPlot <- renderPlot( height = 340, width = 340, {
        # draw the grid
        tmpgrid <- grid()
        if (is.null(tmpgrid)) return (NULL)
        par(mar=c(1,2,2,2))
        plot (tmpgrid, border = spacing(tmpgrid), gridlines = FALSE, bty='o', xaxs='i', yaxs='i')
    })
    ##############################################################################

    output$routePlot <- renderPlot( height = 320, width = 300, {
        # draw the grid
        tmpgrid <- grid()
        if (is.null(tmpgrid)) return (NULL)
        pathl <- gridpathlength()
        seq <- attr(tmpgrid, "seq")
        if (input$returnbox) seq <- c(seq,1)
        par(mar=c(3,0,0,2))
        plot (tmpgrid, border = spacing(tmpgrid), gridlines = FALSE, bty='o', xaxs='i', yaxs='i')
        lines(tmpgrid[seq,], col = 'grey')
        plot(tmpgrid, add = TRUE)  ## overplot symbols
        if (!is.null(pathl))
        mtext(side=1, line=1.5, paste0("Length of route = ", round(pathl/1000,3), " km"),
              xpd = TRUE, adj = 0.5, cex = 1.1)
    })
    ##############################################################################

    output$detnPlot <- renderPlot( height = 290, width = 400, {
        ## inp <- oS2()
        invalidateOutputs()
        
        par(mar=c(4,5,2,5))
        detectfnplot (detectfn = input$detectfn,
                      pars = c(input$lambda0, input$sigma),
                      xval = 0:(3 * input$sigma),
                      ylab = "",
                      hazard = TRUE,       ## 2017-08-28
                      ylim = c(0, input$lambda0*1.2),
                      las=1, col = 'red', lwd = linewidth,
                      xaxs='i', yaxs='i')
        mtext(side = 2, line = 3.7, expression(paste("Detection hazard   ", lambda [0])))
        p <- seq(0,1,0.05)
        axis(4, at = 1 - exp(-p), label = p, xpd = FALSE, las = 1)
        mtext(side = 4, line = 3.7, "Detection probability")
    })
    ##############################################################################

    output$popPlot <- renderPlot( height = 300, width = 380, {
        # draw the grid
        core <- grid()
        if (is.null(core)) return (NULL)
        border <- input$habxsigma * input$sigma
        tmppop <- pop()
        if (is.null(tmppop)) return()
        tmpsig <- input$sigma
        #par(mar=c(0,1,0,1))
        if (input$pxyfillbox) {
            par(mar=c(0,1,0,5)) # , xaxs='i', yaxs='i')
        }
        else {
            par(mar=c(0,3,0,3), xaxs='i', yaxs='i', xpd = FALSE)
        }

        if (input$showmaskbox) {
            plot (core, border = border, gridlines = FALSE)
            plot (mask(), add = TRUE, col = grey(0.9))
            plot(tmppop, add = TRUE, pch = 16, cex = 0.7, xpd = TRUE, frame = FALSE)
            plot (core, add = TRUE)
        }
        else {
            plot (core, border = border, gridlines = FALSE)
            plot(tmppop, pch = 16, cex = 0.7, xpd = TRUE, add = TRUE, frame = FALSE)
            plot (core, add = TRUE)
            bbox <- sweep(apply(core, 2, range), MAR=1, STATS = c(-border,+border), FUN="+")
            bbox <- expand.grid(data.frame(bbox))[c(1,2,4,3),]
            polygon(bbox)
        }
        if (input$showHRbox) {
            # rad <- rep(2.45 * tmpsig, nrow(tmppop))
            rad <- secr::circular.r(p = 0.95, detectfn = input$detectfn, sigma = tmpsig)
            symbols(tmppop$x, tmppop$y, circles = rep(rad, nrow(tmppop)),
                    inches = FALSE, fg = grey(0.7), add = TRUE, xpd = FALSE)
        }
        
    })
    ##############################################################################

    output$pxyPlot <- renderPlot( height = 300, width = 380, {
        
        # draw the grid
        core <- grid()
        if (is.null(core)) return (NULL)
        border <- input$pxyborder * spacing(core)
        ## inp <- oS2()
        invalidateOutputs()
        
        if (input$pxyfillbox) {
            cols <- terrain.colors(11)
            col <- cols[1]
            lev <- c(0.01, seq(0.1, 0.9, 0.1))
            par(mar=c(0,1,0,5)) # , xaxs='i', yaxs='i')
        }
        else {
            col <- "blue"
            cols <- NULL
            lev <- seq(0.1, 0.9, 0.1)
            par(mar=c(0,3,0,3)) # , xaxs='i', yaxs='i')
        }

        plot(core, border = border, gridlines = FALSE, hidetr = TRUE)

        xr <- range(core[,1]) + c(-1,1) * border
        yr <- range(core[,2]) + c(-1,1) * border

        if (input$pxyfillbox) {
            # clunky way to give green background
            rect(xr[1],yr[1],xr[2],yr[2], col = cols[1], border = NA)
            drawlabels <- FALSE
        }
        else
            drawlabels <- input$pxylabelbox

        pdot.contour(core, border = border, nx = input$pxynx,
                     detectfn = input$detectfn,
                     detectpar = list(sigma = input$sigma, lambda0 = input$lambda0),
                     noccasions = input$noccasions, drawlabels = drawlabels,
                     binomN = NULL, levels = lev, poly = poly(),
                     plt = TRUE, add = TRUE,
                     col = col, fill = cols)
        plot (core, add = TRUE)
        if (!is.null(pxyrv$value)) {
            xy <- pxyrv$xy
            points(xy[1], xy[2], pch=16, cex=0.7)
            offset <- (par()$usr[2] - par()$usr[1])/15
            text(xy[1]+offset, xy[2], round(pxyrv$value,3), cex = 0.9, xpd = TRUE)
        }
        if (input$pxyframebox) {
            rect(xr[1],yr[1],xr[2],yr[2], col = NA, border = "black") 
        }
        if (input$pxyfillbox) {
            strip.legend("right", legend = seq(0,1,0.1), title = "p.(x)", xpd = TRUE,
                         legendtype='breaks', inset = 0.01, col = cols[3:12])
        }
    })
    ##############################################################################

    output$powerPlot <- renderPlot( height = 320, width = 360, {
        RSE <- input$RSEslider/100
        if (input$powerplotbtn == "Confidence interval") {
            par(mar=c(3.5,4,2,2), mgp=c(2.4,0.7,0))
            headroom <- (input$maxEffect-input$minEffect)/4
            powLU <- plotpowerCI(RSE = RSE, effectRange=c(input$minEffect, input$maxEffect),
                                 estimatedRange = c(input$minEffect, input$maxEffect+headroom),
                               adjustRSE = input$adjustRSEbox, alpha = input$alpha)
            
        }
        else {
        par(mar=c(4,4,3,2))
        powLU <- plotpower(RSE = RSE, effectRange=c(input$minEffect, input$maxEffect),
                           adjustRSE = input$adjustRSEbox, alpha = input$alpha,
                           testtype = input$testtype,
                           targetpower = input$target)
        }
        ## text (110, 10, paste0("RSE = ", round(100*RSE,1), "%") )
    })
    ##############################################################################

    output$RSEPlot <- renderPlot({
        ## rotrv$output is an object with class "optimalSpacing" with plot method in secrdesign
        if (rotrv$current){
            par(mar=c(5,6,3,6))      
            plot(rotrv$output, plottype = "RSE", col = "blue", lwd = linewidth, cex = 1.1)
        }
        else NULL
    })
    ##############################################################################

    output$nrmPlot <- renderPlot({
        if (rotrv$current){
            par(mar=c(5,6,3,6))
            plot(rotrv$output, plottype = "nrm", col = "blue", lwd = linewidth, cex = 1.1)
        }
        else NULL
    })
    ##############################################################################

    output$costPlot <- renderPlot({
        if (rotrv$current){
            par(mar=c(5,5,3,5))
            cc <- cost(rotrv$output, costs = list(perkm = input$perkm,
                                          perarray = input$perarray,
                                          perdetector = input$perdetector,
                                          pervisit = input$pervisit,
                                          perdetection = input$perdetection))
            maxx <- max(cc$R)
            maxy <- max(cc$totalcost) * 1.2
            par(cex = 1.1)
            defaultargs <- list(x = 0, y = 0, type = "n", las = 1,
                                xlab = expression(paste("Spacing -  ", sigma, "  units")),
                                ylab = expression(paste("Cost $")),
                                ylim = c(0, maxy),
                                xlim = c(0, maxx))

            do.call(plot, defaultargs)
            x <- c(cc$R, rev(cc$R))
            y <- c(cc$detrcost, rep(0, nrow(cc)))
            polygon(x, y, col='green')
            y <- c(cc$detrcost + cc$arraycost, rev(cc$detrcost))
            polygon(x, y, col='blue')
            y <- c(cc$detrcost + cc$arraycost + (cc$pathcost + cc$visitcost), 
                   rev(cc$detrcost + cc$arraycost))
            polygon(x, y, col='orange')
            y <- c(cc$totalcost, rev(cc$detrcost + (cc$pathcost + cc$visitcost) + cc$arraycost))
            polygon(x, y, col='yellow')
            y <- c(cc$totalcost, rep(0, nrow(cc)))
            polygon(x, y, col=NA)
            legend (x = "top", legend = c("detectors","arrays","travel","detections"),
                    fill = c("green","blue","orange","yellow"), horiz = TRUE, cex=0.9)
        }
        else NULL
    })
    ##############################################################################

    ## rendertable
    
    ##############################################################################

    output$summarytable <- renderTable(sumrv$value)

    ##############################################################################
    
    ## downloadhandler
    
    ##############################################################################
    
    output$downloadData <- downloadHandler(
        filename = "summary.csv",
        content = function(file) {
            write.csv(sumrv$value, file, row.names = FALSE)
        }
    )

    ##############################################################################
    # tidy end of session - app closes in R
    
    session$onSessionEnded(function() {
        stopApp()
    })

    ##############################################################################
}


##################################################################################
# Run the application
shinyApp(ui = ui, server = server)
##################################################################################
