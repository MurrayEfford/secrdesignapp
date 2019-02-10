# renamed from secrdesignapp.R

library(secrdesign)

secrversion <- packageVersion('secr')
secrdesignversion <- packageVersion('secrdesign')
if (compareVersion(as.character(secrdesignversion), '2.5.7') < 0)
    stop("secrdesignapp 1.2 requires secrdesign version 2.5.7 or later",
         call. = FALSE)
openCRversion <- packageVersion('openCR')

# requires package rgdal to read shapefiles
# requires package sp for bbox and plot method for SpatialPolygons
# requires package parallel for max cores in simulate options (distributed with base R)
# requires package tools for file path when reading shapefiles (distributed with base R)

# interrupt is hard -
# see http://htmlpreview.github.io/?https://github.com/fellstat/ipc/blob/master/inst/doc/shinymp.html

linewidth <- 2  # for various plots 
seconds <- 6   ## default duration for showNotification()

# Define UI 
ui <- function(request) {

    fluidPage(
        title = "secrdesignapp 1.2",
        includeCSS("secrdesignstyle.css"),
        withMathJax(),
        tags$head(tags$style(".mypanel{margin-top:5px; margin-bottom:10px; padding-bottom: 5px;}")),
        br(),
        navlistPanel(id = "navlist", widths = c(2,10), well=TRUE,
                     
                     "secrdesign app 1.2",
                     
                     tabPanel("Design",
                              fluidRow(
                                  column(3, # offset = 0, style='padding:15px;',
                                         h2("Detector array"),
                                         wellPanel(class = "mypanel", 
                                                   fluidRow(
                                                       column(6, selectInput("detector", "Detector type",
                                                                             # choices = c("multi","proximity","count", "single"),
                                                                             choices = c("multi","proximity","count"),
                                                                             selected = "proximity", width = 120)),
                                                       column(6, uiOutput('detectorhelp'))
                                                   ),
                                                   br(),
                                                   tabsetPanel(
                                                       type = "pills", id = "arrayinput", selected = "Grid",
                                                       
                                                       tabPanel("Grid",
                                                                br(),
                                                                fluidRow(
                                                                    column(5, numericInput("ny",
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
                                                                    column(5,numericInput("spy",
                                                                                          "row space (m)",
                                                                                          value = 20,
                                                                                          min = 1,
                                                                                          max = 100000,
                                                                                          step = 1)),
                                                                    column(6,numericInput("spx",
                                                                                          "col space (m)",
                                                                                          value = 20,
                                                                                          min = 1,
                                                                                          max = 100000,
                                                                                          step = 1))
                                                                ),
                                                                
                                                                fluidRow(
                                                                    column(5, checkboxInput("hollow", "Hollow", FALSE)),
                                                                    column(6, actionButton("suggestbtn", "Suggest spacing",
                                                                           title = "Find detector spacing at which E(n) = E(r)"))
                                                                )
                                                       ),
                                                       tabPanel("Line",
                                                                br(),
                                                                fluidRow(
                                                                    column(7,numericInput("nline",
                                                                                          "number of detectors",
                                                                                          value = 20,
                                                                                          min = 1,
                                                                                          max = 100,
                                                                                          step = 1)),
                                                                    column(5,numericInput("spline",
                                                                                          "spacing (m)",
                                                                                          value = 20,
                                                                                          min = 1,
                                                                                          max = 100000,
                                                                                          step = 1))
                                                                ),
                                                                actionButton("suggestlinebtn", "Suggest spacing",
                                                                           title = "Find detector spacing at which E(n) = E(r)"),
                                                                br(),br(),
                                                                helpText(HTML("Warning: results with linear arrays may be highly biased if home ranges are elongated and aligned"))
                                                       ),
                                                       tabPanel("File", values = "File",
                                                                br(),
                                                                div(style="height: 80px;",
                                                                    fileInput("trapfilename", "",   # Detector layout file
                                                                              accept = "text/plain")),
                                                                helpText(HTML(paste0("Requires text file with detector ID ",
                                                                                     "and x-y coordinates in three columns,",
                                                                                     " as for secr::read.traps"))),
                                                                textInput("args", "Optional arguments for read.traps()",
                                                                          value = "", placeholder = "e.g., skip = 1, sep = ','"),
                                                                br(),
                                                                fluidRow(
                                                                    column (6, numericInput("scalefactor",
                                                                                            "Scaling factor",
                                                                                            value = 1.0,
                                                                                            min = 0, 
                                                                                            max = 100, 
                                                                                            step = 0.01)),
                                                                    column (6, br(), actionButton("suggestfilebtn", "Suggest factor",
                                                                           title = "Find spacing factor at which E(n) = E(r)")))
                                                                
                                                       ),
                                                       tabPanel("Region",
                                                                br(),
                                                                div(style="height: 80px;",
                                                                    fileInput("regionfilename", "Boundary file(s)",
                                                                              accept = c('.shp','.dbf','.sbn','.sbx',
                                                                                         '.shx',".prj", ".txt", ".rdata", 
                                                                                         ".rda", ".rds"), multiple = TRUE)),
                                                                # helpText(HTML("(at least xxx.shp, xxx.dbf, and xxx.shx)")),
                                                                uiOutput("shapefile"),
                                                                
                                                                wellPanel(class = "mypanel", 
                                                                          tabsetPanel(
                                                                              type = "tabs", id = "regiontype", selected = "Random",
                                                                              
                                                                              tabPanel("Random",
                                                                                       fluidRow(
                                                                                           column(9, radioButtons("randomtype", label = "",
                                                                                                        choices = c("SRS", "GRTS"), selected = "SRS", inline = TRUE))
                                                                                       ),
                                                                                       
                                                                                       fluidRow(
                                                                                           column(12, uiOutput('randomtext'))
                                                                                       ),

                                                                                       fluidRow(
                                                                                           
                                                                                           column(6, numericInput("numpgrid",
                                                                                                                        "number",
                                                                                                                        value = 20,
                                                                                                                        min = 0,
                                                                                                                        max = 20000,
                                                                                                                  step = 5)),
                                                                                           column(6, br(), actionButton("randomarraybtn", "Randomize",
                                                                                                                        title = "Select another realisation"))
                                                                                       )
                                                                                       
                                                                              ),
                                                                              tabPanel("Systematic",
                                                                                       
                                                                                       br(),
                                                                                       fluidRow(
                                                                                           column(6,numericInput("sppgrid",
                                                                                                                 "spacing (m)",
                                                                                                                 value = 200,
                                                                                                                 min = 0,
                                                                                                                 max = 200000,
                                                                                                                 step = 10)),
                                                                                           column(6, 
                                                                                                  checkboxInput("randomorigin", "Random origin", FALSE),
                                                                                                  checkboxInput("chequerboard", "Chequerboard", FALSE))
                                                                                           
                                                                                       ),
                                                                                       uiOutput('clusteroverlap'))
                                                                              
                                                                              
                                                                          )),
                                                                wellPanel(class = "mypanel",
                                                                          fluidRow(column(7, 
                                                                                          radioButtons("clustertype", label = "Cluster type",
                                                                                                       choices = c("Single detector", "Grid", "Line", "File"), 
                                                                                                       selected = "Single detector")),
                                                                                   column(5, 
                                                                                          numericInput(
                                                                                              "rotation",
                                                                                              "Rotation",
                                                                                              value = 0,
                                                                                              min = 0,
                                                                                              max = 360,
                                                                                              step = 5),
                                                                                          numericInput(
                                                                                              "seedpgrid", 
                                                                                              "Seed",
                                                                                              value = 0,
                                                                                              min = 0, 
                                                                                              max = 1000000000,
                                                                                              step = 1)
                                                                                   )))
                                                                
                                                       )
                                                   )
                                         )
                                  ),
                                  
                                  column(3, # offset = 0, style='padding:0px;',  
                                         
                                         h2("Parameters"),
                                         wellPanel(class = "mypanel", 
                                                   
                                                   fluidRow(
                                                       column(6, numericInput("D",
                                                                              "D (animals / ha)",
                                                                              min = 0,
                                                                              max = 1000,
                                                                              value = 5,
                                                                              step=0.1,
                                                                              width = 180)
                                                              # uiOutput('persqkm')
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
                                                                              max = 100000,
                                                                              value = 25,
                                                                              step = 1,
                                                                              width = 180)))
                                         ),
                                         
                                         h2("General"),
                                         
                                         fluidRow(
                                             column(6,
                                                    wellPanel(class = "mypanel", 
                                                              numericInput("noccasions",
                                                                           "Occasions",
                                                                           value = 5,
                                                                           min = 1,
                                                                           max = 100,
                                                                           step = 1,
                                                                           width = 220),
                                                              numericInput("nrepeats",
                                                                           "Arrays",
                                                                           value = 1,
                                                                           min = 1,
                                                                           max = 100,
                                                                           step = 1,
                                                                           width = 220)
                                                              # uiOutput('clusterhelp')
                                                    )),
                                             column(6, #offset = 1, style='padding:0px;',
                                                    wellPanel(class = "mypanel", 
                                                              radioButtons("distributionbtn", label = "Distribution of n",
                                                                           choices = c("Poisson", "Binomial"))
                                                    ),
                                                    checkboxInput("autorefresh", "  Auto refresh", TRUE)
                                                    # checkboxInput("autoappend", "  Add on refresh", FALSE)
                                             )
                                         ),
                                         
                                         h2("Actions"),
                                         
                                         fluidRow(
                                             column(5, actionButton("simulatebtn2", "Simulate",  width = 130,
                                                                           title = "Conduct simulations specified on Simulate page and update Results")),
                                             column(6, actionButton("appendbtn", "Add to summary",  width = 130,
                                                                           title = "Append new scenario to Summary table"))
                                         ),

                                         br(),
                                         fluidRow(
                                             column(5, actionButton("resetbtn", "Reset all", width = 130, 
                                                                    title = "Reset all inputs to initial values")),
                                             column(6, bookmarkButton(width = 130)) 
                                         ),
                                         br(),
                                         fluidRow(
                                             column(7, helpText(HTML("F11 to toggle fullscreen")))
                                         ),
                                         fluidRow(
                                             column(11, textInput("title", "", value = "", 
                                                                  placeholder = "scenario label for Summary")))
                                  ),
                                  
                                  column (4, # style='padding:0px;',
                                          h2("Results"),
                                          
                                          fluidRow(
                                              column(12,
                                                     fluidRow(
                                                         column(11, verbatimTextOutput("nrmPrint")),
                                                         column(1, style='padding:0px;',
                                                                conditionalPanel("output.nrmPrint!= ''",
                                                                                 downloadLink("downloadnrmcode", "R")),
                                                                br(),
                                                                conditionalPanel("output.nrmPrint!= ''",
                                                                                 plotOutput("trafficlightPlot", height = 60)))
                                                     )
                                              )
                                          ),
                                          
                                          fluidRow(
                                              column(12,
                                                     br(),
                                                     tabsetPanel(type = "pills",
                                                                 id = "tabs",
                                                                 tabPanel("Array",
                                                                          fluidRow(
                                                                              column(9, style='padding:0px;', plotOutput("arrayPlot", height = 340,
                                                                                                                         click = clickOpts(id="arrayClick", clip = FALSE))),
                                                                              column(3, br(), conditionalPanel("input.gridlines != 'None'",
                                                                                                              uiOutput("uigridlines") ),
                                                                                     br(), uiOutput('xycoord'))
                                                                          ),
                                                                          fluidRow(
                                                                              column(11, style='padding:0px;', verbatimTextOutput("ntrapPrint"))
                                                                              ,
                                                                              column(1, br(), conditionalPanel("output.ntrapPrint!= ''",
                                                                                                               downloadLink("downloadArray", "Save")))
                                                                          )
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
                                                                              column(4, actionButton("randompopbtn", "Randomize",
                                                                                                     title = "Pick another realisation of the population")
                                                                              )
                                                                          )
                                                                 ),
                                                                 tabPanel("Pxy",
                                                                          plotOutput("pxyPlot", height = 320, click = "pxyclick"),
                                                                          helpText(HTML("p.(x) is the probability an animal at point x will be detected at least once"))
                                                                 ),
                                                                 
                                                                 tabPanel("Power",
                                                                          fluidRow(
                                                                              column(11, plotOutput("powerPlot", height = 320, click = "CIclick"))
                                                                          ),
                                                                          br(),
                                                                          fluidRow(
                                                                              column(3, offset = 1,
                                                                                     br(), checkboxInput("powertype", "95% CI",
                                                                                                         value = TRUE,
                                                                                                         width = 130)),
                                                                              ## uiOutput('CIpct'),
                                                                              column(4, 
                                                                                     br(), checkboxInput("adjustRSEbox", "Adjust final RSE",
                                                                                                         value = TRUE,
                                                                                                         width = 130)),
                                                                              # helpText(HTML("Scales with population"))),
                                                                              column(4, conditionalPanel("input.powertype==true",
                                                                                                         numericInput("xpos", "% change", min = -100, max = 250, 
                                                                                                                      step = 1, value = 0, width = 70)
                                                                                                         )
                                                                                     )
                                                                              
                                                                          ),
                                                                          fluidRow(
                                                                              column(12,
                                                                                     sliderInput("RSEslider", "",
                                                                                                 min = 1.0,
                                                                                                 max = 40,
                                                                                                 value = 1,
                                                                                                 step = 0.1,
                                                                                                 pre = "RSE ",
                                                                                                 post = "%",
                                                                                                 width = "90%"))
                                                                          )
                                                                 )
                                                     ))
                                          )
                                  )
                              )
                     ),
                     
                     tabPanel("Costing",
                              fluidRow(
                                  column(3,
                                         h2("Unit cost"),
                                         wellPanel(class = "mypanel", 
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
                                                                "Cost per detector visit $",
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
                                                                width = 200)),
                                         br(),
                                         fluidRow(
                                             column(10, offset=1, checkboxInput("setupoccasion", "Include setup occasion", TRUE))
                                         )
                                  ),
                                  column(3, offset = 0,
                                         h2("Route"),
                                         wellPanel(class = "mypanel", 
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
                                  column(5,
                                         h2("Simulation control"),
                                         fluidRow(
                                             column(6, 
                                                    wellPanel(class = "mypanel", 
                                                              radioButtons("packagebtn", label = "Function to fit SECR model", 
                                                                           choices = c("openCR.fit", "secr.fit", "no fit")),
                                                              selectInput("method", "Maximization method",
                                                                          choices = c("Newton-Raphson", "Nelder-Mead", "none"),
                                                                          selected = "none", width=160),
                                                              checkboxInput("simappendbox", "Add to Summary", TRUE)
                                                    )
                                             ),
                                             column(6,
                                                    wellPanel(class = "mypanel", 
                                                              
                                                              fluidRow(
                                                                  column(6, numericInput("nrepl",
                                                                                         "Replicates",
                                                                                         min = 1,
                                                                                         max = 1000,
                                                                                         value = 5,
                                                                                         step = 1,
                                                                                         width = 180)),
                                                                  column(6, numericInput("ncores",
                                                                                         "Cores",
                                                                                         min = 1,
                                                                                         max = parallel::detectCores(),
                                                                                         value = 1,
                                                                                         step = 1,
                                                                                         width = 180))),
                                                              fluidRow(
                                                                  column(6, numericInput("seed",
                                                                                         "Random seed",
                                                                                         min = 0,
                                                                                         max = 1e10,
                                                                                         value = 0,
                                                                                         step = 1,
                                                                                         width = 180))
                                                                  # ,
                                                                  # column(6, numericInput("simnx",
                                                                  #                        "nx",
                                                                  #                        min = 10,
                                                                  #                        max = 1000,
                                                                  #                        value = 32,
                                                                  #                        step = 1,
                                                                  #                        width = 180))
                                                              )
                                                    ),
                                                    
                                                    br(),
                                                    actionButton("simulatebtn", "Click to execute", width = 200)
                                                    
                                             )
                                         ),
                                         
                                         h2("Standalone R code"),
                                         verbatimTextOutput("simcodePrint")
                                         
                                  ),
                                  column(5,
                                         h2("Results"),
                                         fluidRow(
                                             column(9, verbatimTextOutput("simPrint"))
                                         )
                                  )
                              )
                     ),
                     
                     #################################################################################################
                     
                     tabPanel("Spacing",
                              fluidRow(
                                  column(5,
                                         fluidRow(
                                             column(12, h2("Standalone R code"),
                                                    verbatimTextOutput("spacingcodePrint"))
                                         ),
                                         
                                         fluidRow(
                                             column(6, 
                                                    actionButton("spacingbtn", "Click to execute", width = 200,
                                                                 title = "Approximate RSE and optionally simulate at spacings selected in Options")
                                             ),
                                             column(6, 
                                                    checkboxInput("spacingsimbox", "with simulations", value = FALSE, width = 200),
                                                    helpText(HTML('See Simulation for control options'))
                                             )
                                         )
                                  ),
                                  column(5,
                                         h2("Results"),
                                         fluidRow(
                                             column(10, verbatimTextOutput("spacingPrint"))
                                             ,
                                             column(2, conditionalPanel("output.validspacing==true",
                                                                        downloadLink("downloadSpacing", "Save oS")))
                                         ),
                                         
                                         fluidRow(
                                             column(9, verbatimTextOutput("spacingPrint2"))
                                         ),
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
                                                          column(10,verbatimTextOutput("nrmlegend"))
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
                              br(),
                              fluidRow(
                                  column(3, 
                                         wellPanel(
                                             h2("Fields"),
                                             fluidRow(
                                                 column(4, actionButton("selectfieldsbtn", "Select", title = "Choose fields to display")), 
                                                 column(4, actionButton("selectnonebtn", "None", title = "Unselect all fields")), 
                                                 column(4, actionButton("selectallbtn", "All", title = "Select all fields"))
                                             ),
                                             br(),
                                             h2("Scenarios"),
                                             fluidRow(
                                                 column(6, actionButton("clearallbtn", "Clear all", title = "Delete all scenarios")), 
                                                 column(6, actionButton("clearlastbtn", "Delete last", title = "Delete last scenario"))
                                             ), 
                                             br(), 
                                             h2("Download"),
                                             fluidRow(
                                                 column(6, downloadButton("downloadSummaryrds", ".rds", 
                                                                                      title = "Save as RDS file; restore in R with e.g., readRDS(`summary.rds')")), 
                                                 column(6, downloadButton("downloadSummary", ".csv", 
                                                                                    title = "Save as comma-delimited text (csv) file"))
                                             )
                                         ),
                                         
                                         fluidRow(
                                             column(5, offset=1,
                                                    conditionalPanel("output.selectingfields == 'TRUE'",
                                                                     checkboxGroupInput("fields1", "",
                                                                                        choices = c("date", "time", "note", "detector", "source", "nx", "ny", "spacex", "spacey",
                                                                                                    "ndetectors", "noccasions", "nrepeats", "distribution", "detectfn",
                                                                                                    "D", "lambda0", "sigma"),
                                                                                        selected = c("date", "time", "note", "detector", "source", "nx", "ny", "spacex", "spacey",
                                                                                                     "ndetectors", "noccasions", "nrepeats", "distribution", "detectfn",
                                                                                                     "D", "lambda0", "sigma")
                                                                     ))),
                                             column(6, 
                                                    conditionalPanel("output.selectingfields == 'TRUE'",
                                                                     checkboxGroupInput("fields2", "",
                                                                                        choices = c("detperHR", "k", "En", "Er", "Em",
                                                                                                    "rotRSE", "CF", "route", "cost", "simfn",  "nrepl", "simtime",
                                                                                                    "simRSE", "simRSEse", "simRB", "simRBse"),
                                                                                        selected = c("detperHR", "k", "En", "Er", "Em",
                                                                                                     "rotRSE", "CF", "route", "cost", "simfn",  "nrepl", "simtime",
                                                                                                     "simRSE", "simRSEse", "simRB", "simRBse")
                                                                     )
                                                    )                                                  
                                             )
                                         )
                                         
                                  ),
                                  column(9, 
                                         # h2("Results"),
#                                         div(tableOutput("summarytable"), style = "width:800px; overflow-x: scroll; font-size:80%")
                                         div(tableOutput("summarytable"), style = "width:800px; overflow-x: scroll")
                                  )
                              )
                     ),
                     #################################################################################################
                     
                     tabPanel("Options",
                              
                              fluidRow(
                                  column(3,
                                         
                                         h2("Detector array"),
                                         wellPanel(class = "mypanel", 
                                                   fluidRow(
                                                       column(6, radioButtons("areaunit", label = "Area units",
                                                                              choices = c("ha", "km^2"), 
                                                                              selected = "ha", inline = TRUE))
                                                   ),
                                                   br(),
                                                   radioButtons("edgemethod", label = "Method for clusters at edge of region",
                                                                choices = c("clip", "anyinside", "allinside"),
                                                                selected = "clip", inline = TRUE),
                                                   br(),
                                                   div(style="height: 80px;",
                                                       fileInput("exclusionfilename", "Excluded region",
                                                                 accept = c('.shp','.dbf','.sbn','.sbx',
                                                                            '.shx',".prj", ".txt", ".rdata", ".rda", ".rds"), 
                                                                 multiple = TRUE)),
                                                   uiOutput("exclusionfile"),
                                                   fluidRow(
                                                       column(9, offset=1,
                                                              div(style="height: 20px;", checkboxInput("exclusionbox", "Apply exclusion", TRUE)))),
                                                   br(),
                                                   fluidRow(
                                                       column(6, numericInput("maxupload", "Maximum file upload Mb",
                                                                min = 5,
                                                                max = 100,
                                                                value = 5,
                                                                step = 5)),
                                                       column(6,numericInput("maxdetectors", "Maximum detectors",
                                                                             min = 0,
                                                                             max = 20000,
                                                                             value = 1000,
                                                                             step = 100))
                                                       ),
                                                   checkboxInput("lockxy", "Couple row and column dimensions", TRUE)
                                         ),
                                         
                                         h2("Habitat mask"),
                                         wellPanel(class = "mypanel", 
                                                   fluidRow(
                                                       column(6, 
                                                              numericInput("habxsigma", "Buffer width (x sigma)",
                                                                           min = 0,
                                                                           max = 20,
                                                                           value = 4,
                                                                           step = 0.5,
                                                                           width = 250),
                                                              numericInput("habnx", "Mesh dimension nx",
                                                                           min = 10,
                                                                           max = 1000,
                                                                           value = 32,
                                                                           step = 1,
                                                                           width = 180)
                                                       ),
                                                       column(6, 
                                                              radioButtons("maskshapebtn", label = "Shape",
                                                                           choices = c("Rectangular", "Rounded"), 
                                                                           selected = "Rounded")
                                                       )
                                                   ),
                                                   br(),
                                                   div(style="height: 80px;",
                                                       fileInput("habpolyfilename", "Mask polygon file(s)",
                                                                 accept = c('.shp','.dbf','.sbn','.sbx',
                                                                            '.shx',".prj", ".txt", ".rdata", ".rda", ".rds"), 
                                                                 multiple = TRUE)),
                                                   uiOutput("habitatfile"),
                                                   fluidRow(
                                                       column(10, offset = 1, div(style="height: 20px;",
                                                                                 checkboxInput("polygonbox", "Clip to polygon(s)", value = TRUE, width = 180)))
                                                   ),
                                                   fluidRow(
                                                       column(10, offset = 1, radioButtons("includeexcludebtn", label = "",
                                                                                          choices = c("Include", "Exclude"), 
                                                                                          selected = "Include", inline = TRUE))
                                                   )
                                         )
                                  ),
                                  
                                  column(3,
                                         
                                         h2("Array plot"),
                                         wellPanel(class = "mypanel", 
                                                   radioButtons("gridlines", label = "Grid spacing (m)",
                                                                choices = c("None", "100", "1000", "10000", "100000"),
                                                                selected = "None", inline = TRUE),
                                                   fluidRow(
                                                       column(6, checkboxInput("entireregionbox", "Show entire region", value = TRUE, width = 160)),
                                                       column(5, checkboxInput("snaptodetector", "Snap to detector", value = FALSE, width = 160))
                                                   )
                                         ),
                                         h2("Pxy contour plot"),
                                         wellPanel(class = "mypanel", 
                                                   fluidRow(
                                                       column(6, numericInput("pxyborder", "Border (spacing units)",
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
                                                                           width = 180)),
                                                       column(6,
                                                              br(),
                                                              checkboxInput("pxyfillbox", "Fill contours",
                                                                            value = TRUE,
                                                                            width = 180),
                                                              checkboxInput("pxyframebox", "Show frame",
                                                                            value = FALSE,
                                                                            width = 180),
                                                              checkboxInput("pxylabelbox", "Label contours",
                                                                            value = TRUE,
                                                                            width = 180))
                                                   )
                                         ),
                                         h2("Spacing plot"),
                                         wellPanel(class = "mypanel", 
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
                                         ),
                                         h2("Costing"),
                                         wellPanel(class = "mypanel",
                                                   radioButtons("currency", label = "Currency symbol",
                                                                choices = c("$", "\U00A3", "\U00A5", 
                                                                            "\U20AC", "Rs"),  # , "\U20A8" rupees fail
                                                                selected = "$", inline = TRUE)
                                         )
                                  ),
                                  
                                  column(3,
                                         h2("Power plot"),
                                         wellPanel(class = "mypanel", 
                                                   fluidRow(
                                                       column(6, numericInput("alpha", "alpha",
                                                                              min = 0.001,
                                                                              max = 0.200,
                                                                              value = 0.05,
                                                                              step = 0.001,
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
                                                   
                                                   br(),
                                                   h4("Hypothesis test"),
                                                   # br(),
                                                   fluidRow(
                                                       column(6,selectInput("testtype", "Type",
                                                                            choices = c("two.sided", "decrease", "increase"),
                                                                            selected = "two.sided",
                                                                            width = 140)),
                                                       column(6, numericInput("target", "Target power %",
                                                                              min = 50,
                                                                              max = 99,
                                                                              value = 80,
                                                                              step = 1,
                                                                              width = 180))
                                                   )
                                         ),
                                         h2("RSE correction factor"),
                                         wellPanel(class = "mypanel", 
                                                   sliderInput("CFslider", "",
                                                               min = 0.8,
                                                               max = 2.0,
                                                               value = 1,
                                                               step = 0.001,
                                                               pre = "CF ",
                                                               post = "",
                                                               width = 340),
                                                   fluidRow(
                                                       column(8, checkboxInput("updateCFbox", "Update from simulations",
                                                                               value = TRUE,
                                                                               width = 180)),
                                                       column(4, actionButton("resetCFbtn", "Reset", title = "Reset slider to default for current array input"))
                                                   )
                                         )
                                  )
                              )
                     ),
                     tabPanel("Help",
                              withMathJax(includeMarkdown("help.rmd"))
                     ),
                     tabPanel("About",
                              h2("secrdesign app 1.2"), br(),
                              
                              h5(paste("This Shiny application provides an interface to the R package 'secrdesign', version", 
                                       packageDescription("secrdesign")$Version), "."),
                              br(),
                              h5("Copyright 2019 Murray Efford"),
                              h5("The application is released under the"),
                              a("GNU General Public License Version 3.0", href="https://www.gnu.org/licenses/gpl-3.0.txt", target="_blank"), br(),
                              br(),
                              h5("For further information see "), 
                              a("www.otago.ac.nz/density", href="https://www.otago.ac.nz/density", target="_blank"), br(),
                              a("CRAN.R-project.org/package=secrdesign", href="https://CRAN.R-project.org/package=secrdesign", target="_blank"), br(),
                              a("https://github.com/MurrayEfford/secrdesignapp", href="https://github.com/MurrayEfford/secrdesignapp", target="_blank"), br(),
                              br(),
                              
                              h5("Citation"),
                              h5("[The preferred citation for this package is not finalised]"),
                              h5("Efford, M. G. 2019. Fast evaluation of study designs for spatially explicit capture-recapture. In prep.")
                     )
                     
        )
    )
}
############################################################################################
# Define server logic

server <- function(input, output, session) {
    
    desc <- packageDescription("secrdesign")
    summaryfields <- c("date", "time", "note", "detector", "source", "nx", "ny", "spacex", "spacey",
                       "ndetectors", "noccasions", "nrepeats", "distribution", "detectfn", 
                       "D", "lambda0", "sigma", "detperHR", "k", "En", "Er", "Em",
                       "rotRSE", "CF", "route", "cost", "simfn",  "nrepl", "simtime", 
                       "simRSE", "simRSEse", "simRB", "simRBse")
    fieldgroup1 <- 1:17
    fieldgroup2 <- 18:33
    simfields <- summaryfields[27:33]
    
    showNotification(paste("secrdesign", desc$Version, desc$Date),
                     closeButton = FALSE, type = "message", duration = seconds)
     output$selectingfields <- renderText('false')
     outputOptions(output, "selectingfields", suspendWhenHidden = FALSE)
    ##############################################################################
    
    ## renderUI
    
    ## persqkm
    ## detectorhelp
    ## clusterhelp
    ## clusteroverlap
    ## randomtext
    ## shapefile
    ## exclusionfile
    ## habitatfile
    ## uipopN
    ## uigridlines
    
    ##############################################################################
    
    output$persqkm <- renderUI({
        ## display density in animals/km^2
        Dkm <- density() * 100
        Dkmtext <- paste0(Dkm, '&nbsp; animals / km<sup>2</sup>')
        helpText(HTML(Dkmtext))
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
        else if (input$detector == 'single')
            helptext <- "single-catch trap; max. one detection per animal & one per trap on any occasion"
        helpText(HTML(helptext))
    })
    ##############################################################################
    
    output$clusterhelp <- renderUI({
        helptext <- "One array"
        if (input$arrayinput!='Region' & input$nrepeats>1)
            helptext <- "Repeated arrays"
        else if (input$arrayinput=='Region')
            helptext <- "Not applicable"
        helpText(HTML(helptext))
    })
    ##############################################################################
    
    output$clusteroverlap <- renderUI({
        helptext <- ""
        if ((input$arrayinput=='Region') & input$clustertype == "Grid") {
            if ((input$spx * input$nx >= input$sppgrid) |
                (input$spy * input$ny >= input$sppgrid))
                helptext <- "Warning: clusters overlap"
        }
        helpText(HTML(helptext))
    })
    ##############################################################################
    
    output$randomtext <- renderUI({
        helptext <- ""
        if (input$arrayinput=='Region' & input$regiontype=="Random") {
            if (input$randomtype == "SRS")
                helptext <- "Simple random sample"
            else if (input$randomtype == "GRTS")
                helptext <- "Generalised random tessellation stratified sample of Stevens & Olsen (2004)"
        }
        helpText(HTML(helptext))
    })
    ##############################################################################
    
    output$shapefile <- renderUI({
        helptext <- ""
        if (!is.null(input$regionfilename)) {
            pos <- grep(".shp", tolower(input$regionfilename[,1]))
            if (length(pos)>0)
                helptext <- paste0(input$regionfilename[pos,1])
            pos <- grep(".rda", tolower(input$regionfilename[,1]))  # .rda, .rdata
            if (length(pos)>0) {
                objlist <- load(input$regionfilename[1,4])
                helptext <- paste0(objlist[1])
            }
            pos <- grep(".rds", tolower(input$regionfilename[,1])) 
            if (length(pos)>0) {
                helptext <- paste0(input$regionfilename[pos,1])
            }
        }
        helpText(HTML(helptext))
    })
    ##############################################################################
    
    output$exclusionfile <- renderUI({
        helptext <- ""
        if (!is.null(input$exclusionfilename)) {
            pos <- grep(".shp", tolower(input$exclusionfilename[,1]))
            if (length(pos)>0)
                helptext <- paste0(input$exclusionfilename[pos,1])
            pos <- grep(".rda", tolower(input$exclusionfilename[,1]))  # .rda, .rdata
            if (length(pos)>0) {
                objlist <- load(input$exclusionfilename[1,4])
                helptext <- paste0(objlist[1])
            }
            pos <- grep(".rds", tolower(input$exclusionfilename[,1])) 
            if (length(pos)>0) {
                helptext <- paste0(input$exclusionfilename[pos,1])
            }
        }
        helpText(HTML(helptext))
    })
    ##############################################################################
    
    output$habitatfile <- renderUI({
        helptext <- ""
        if (!is.null(input$habpolyfilename)) {
            pos <- grep(".shp", tolower(input$habpolyfilename[,1]))
            if (length(pos)>0)
                helptext <- paste0(input$habpolyfilename[pos,1])
            pos <- grep(".rda", tolower(input$habpolyfilename[,1]))  # .rda, .rdata
            if (length(pos)>0) {
                objlist <- load(input$habpolyfilename[1,4])
                helptext <- paste0(objlist[1])
            }
            pos <- grep(".rds", tolower(input$habpolyfilename[,1])) 
            if (length(pos)>0) {
                helptext <- paste0(input$habpolyfilename[pos,1])
            }
        }
        helpText(HTML(helptext))
    })
    ##############################################################################
    
    output$uipopN <- renderUI({
        n <- if (is.null(pop())) 0 else nrow(pop())
        helpText(HTML(paste0("Number in mask = ", n)))
    })
    
    output$uigridlines <- renderUI({
        if(input$gridlines=="None")
            helpText("")
        else if (input$gridlines=="100")
            helpText(span(style="color:gray", HTML("100-m grid")))
        else 
            helpText(span(style="color:gray", HTML(paste0(round(as.numeric(input$gridlines)/1000,1), "-km grid"))))
    })
    
    output$xycoord <- renderUI({
        xy <- c(input$arrayClick$x, input$arrayClick$y)
        tmpgrid <- isolate(detectorarray())
        if (is.null(xy)) 
            helpText("")
        else {
            if (input$snaptodetector) {
                nearest <- nearesttrap(xy, tmpgrid)
                xy <- tmpgrid[nearest,]
                id <- paste0(rownames(tmpgrid)[nearest], ":")
            }
            else {
                id <- ""
            }
            helpText(HTML(paste(id, paste(round(xy), collapse = ", "))))
        }
    })
    
    ##############################################################################
    ## miscellaneous functions
    
    ## pathlength
    ## cost
    ## plotpower    
    ## plotpowerCI    
    ## addsequence
    ## n.eq.r
    ## nrepeats
    ## runsims
    ## runspacing
    ## readpolygon
    ## addtosummary
    ## arraycode
    ## maskcode
    ## nrmcode    
    ## areastr    format area with units
    ## density    density in animals / ha
    
    ##############################################################################
    
    areastr <- function (area) {
        if (area<1000) 
            dec <- 2
        else 
            dec <- 0
        if (isolate(input$areaunit) == "ha") {
            paste0(round(area, dec), " ha")
        }
        else {
            paste0(round(area/100, dec), " km^2")
        }
    }
    ##############################################################################
    
    lengthstr <- function (length, dec) {
        a.unit <- isolate(input$areaunit)
        lth <- if (a.unit == "ha") length else length/1000
        
        if (missing(dec)) {
            if (lth<1000) 
                dec <- 2
            else 
                dec <- 0
        }
        if (a.unit == "ha") {
            paste0(round(lth, dec), " m")
        }
        else {
            paste0(round(lth, dec), " km")
        }
    }
    ##############################################################################
    
    shpfile <- function (filename) {
        if (is.null(filename)) 
            FALSE 
        else {
            if (is.data.frame(filename))
                filename <- filename[,1]
            length(grep(".shp", tolower(filename)))>0
        }
    }
    ##############################################################################

    density <- function() {
        ## return density in animals / hectare
        if (input$areaunit == "ha")
            input$D
        else
            input$D/100  ## per sq. km
    }
    ##############################################################################
    
    cost <- function (x, costs = NULL) {
        if (!inherits(x, "optimalSpacing"))
            stop("requires input of class optimalSpacing")
        if (is.null(costs))
            stop("provide costs as list with components 'perkm', 'perarray', ", 
                 "'perdetector', 'pervisit', 'perdetection'")
        df <- x$rotRSE$values
        df$C <- apply(df[, c("n","r")],1,sum)
        trps <- attr(x, "traps")
        sig <- attr(x, "detectpar")$sigma
        nocc <- attr(x, "noccasions")
        nrep <- attr(x, "nrepeats")
        
        ## initial path length in sigma units
        pathl <- arraypathlength()
        spc <- spacing(trps); if (is.null(spc)) spc <- NA
        df$pathlength <- df$R * pathl / spc * sig / 1000
        
        df$pathcost  <- costs$perkm * df$pathlength * (nocc+1) * nrep
        df$arraycost <- costs$perarray * nrep
        df$detrcost  <- costs$perdetector * nrow(trps) * nrep
        df$visitcost <- costs$pervisit * nrow(detectorarray()) * (nocc+1) * nrep
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
    
    nrepeats <- function() {
        if (input$arrayinput=='Region') 1 
        else input$nrepeats
    }
    ##############################################################################
    
    n.eq.r <- function () {
        ## find minimum at n = r
        
        nminr <- function(R) {
            if (arrinput == "Grid") {
                array <- make.grid(nx = input$nx, ny = input$ny, detector = input$detector,
                                   spacex = input$spx*R, spacey = input$spy*R,
                                   hollow = input$hollow)
            }
            else if (arrinput == "Line") {
                array <- make.grid(nx = input$nline, ny = 1, detector = input$detector,
                                   spacing = input$spline*R)
            }
            else if (arrinput == "File") {
                array <- array0
                array[,] <- array[,] * R
            }
            msk <- suppressWarnings(make.mask(array, buffer = input$habxsigma * input$sigma, nx = input$habnx))
            nrm <- Enrm(density(), array, msk, detectpar, input$noccasions, input$detectfn)
            nrm[1] - nrm[2]
        }
        
        arrinput <- isolate(input$arrayinput)
        if (!(arrinput %in% c("Grid", "Line", "File"))) {
            stop ("optimal spacing works only for Grid, Line or File")
        }
        detectpar <- list(lambda0 = input$lambda0, sigma = input$sigma)
        if (nrow(detectorarray()) > 1) {
            if (arrinput == "File") {
                array0 <- readtrapfile(1)
            }
            if (arrinput %in% c("Grid", "Line")) {
                lower <- input$fromR
                upper <- input$toR
            }
            else {
                lower <- 0.01
                upper <- 100
            }
            R <- try(uniroot(nminr, interval = c(lower, upper)))
            if (inherits(R, "try-error") || R$iter==0) {
                showNotification("optimal value not found", type = "error", id = "nooptimum", duration = seconds)
                return(NA)
            }
            else {
                
                if (arrinput == "Grid") 
                    round(R$root * isolate(input$spx),1)
                else if (arrinput == "Line") 
                    round(R$root * isolate(input$spline),1)
                else
                    return(R$root)
            }
        }
        else NA
    }
    ##############################################################################
    
    readpolygon <- function (fileupload) {
        poly <- NULL
        if (!is.null(fileupload) & is.data.frame(fileupload))
        {
            if (!file.exists(fileupload[1,4])) {
                return(NULL)   ## protect against bad shapefile
            }
            ext <- tolower(tools::file_ext(fileupload[1,1]))
            if (ext == "txt") {
                coord <- read.table(fileupload[1,4])
                poly <- secr:::boundarytoSP(coord[,1:2])
            }
            else if (ext %in% c("rdata", "rda", "rds")) {
                if (ext == "rds") {
                    obj <- readRDS(fileupload[1,4])
                }
                else {
                    objlist <- load(fileupload[1,4])
                    obj <- get(objlist[1])
                }
                if (is.matrix(obj))
                    poly <- secr:::boundarytoSP(obj[,1:2])
                else if (inherits(obj, "SpatialPolygons"))
                    poly <- obj
                else stop("unrecognised boundary object in ", objlist[1])
            }
            else {
                ## not working on restore bookmark 2019-01-24
                dsnname <- dirname(fileupload[1,4])
                for ( i in 1:nrow(fileupload)) {
                    file.rename(fileupload[i,4], paste0(dsnname,"/",fileupload[i,1]))
                }
                filename <- list.files(dsnname, pattern="*.shp", full.names=FALSE)
                layername <- tools::file_path_sans_ext(basename(filename))
                if (is.null(filename) || 
                    !(any(grepl(".shp", fileupload[,1])) &&
                      any(grepl(".dbf", fileupload[,1])) &&
                      any(grepl(".shx", fileupload[,1])))) {
                    showNotification("need shapefile components .shp, .dbf, .shx",
                                     type = "error", id = "nofile", duration = seconds)
                }
                else  if (!requireNamespace("rgdal"))
                    showNotification("need package rgdal to read shapefile", type = "error", id = "norgdal", duration = seconds)
                else {
                    removeNotification(id = "nofile")
                    removeNotification(id = "norgdal")
                    poly <- rgdal::readOGR(dsn = dsnname, layer = layername)
                }
            }
        }
        poly   
    }
    ##############################################################################
    
    runsims <- function() {
        
        progress <- Progress$new(session, min=1, max=15)
        on.exit(progress$close())
        progress$set(message = 'Simulating...',
                     detail = '')
        seed <- input$seed
        if (seed == 0) seed <- NULL
        array <- detectorarray()
        msk <- mask()
        Ndist <- if (input$distributionbtn == 'Poisson') 'poisson' else 'fixed'
        fit <- input$packagebtn %in% c('openCR.fit', 'secr.fit')
        fitargs = list(detectfn = input$detectfn, 
                       method = input$method, 
                       details = list(distribution= tolower(input$distributionbtn)))
        if (input$packagebtn == "openCR.fit")
            fitargs$distribution <- tolower(input$distributionbtn)
        scen <- make.scenarios(
            noccasions = input$noccasions,
            nrepeats = nrepeats(),
            D = density(),
            lambda0 = input$lambda0,
            sigma = input$sigma,
            detectfn = input$detectfn,
            recapfactor = 1)
        sims <- run.scenarios (
            nrepl = input$nrepl,
            scenarios = scen,
            trapset = array,
            maskset = msk,
            fit = fit,
            fit.function = if(fit) input$packagebtn else NULL,
            extractfn = summary,
            pop.args = list(Ndist = Ndist),
            fit.args = if (fit) fitargs else NULL,
            ncores = input$ncores,
            byscenario = FALSE,
            seed = seed,
            terse = TRUE, moves = TRUE)  ## arguments for summary.capthist
        
        if (fit) {
            if ((input$packagebtn == "openCR.fit") && compareVersion(as.character(openCRversion), '1.3.3') < 0) {
                showNotification("Moves NA with fit, openCR < 1.3.3",
                                 type = "warning", id = "movesNA", duration = seconds)
            }
            counts <- summary(count(sims))$OUTPUT[[1]]
        }
        else {  
            sumc <- function(x) {
                c(n = sum(!is.na(x)),
                  mean = mean(x, na.rm = TRUE),
                  se =  sd(x, na.rm = TRUE)/ sum(!is.na(x))^0.5)
            }
            counts <- t(apply(sims$output[[1]], 2, sumc))
        }
        ## store simulation results in reactive value
        simrv$output <- list(
            nrepl = input$nrepl,
            method = input$method,
            fit = fit,
            proctime = sims$proctime,
            n = counts['Animals', 'mean'],
            n.se = counts['Animals', 'se'],
            ndet = counts['Detections', 'mean'],
            ndet.se = counts['Detections', 'se'],
            nmov = counts['Moves', 'mean'],
            nmov.se = counts['Moves', 'se'])
        if (fit) {
            predicted <- summary(predict(sims))$OUTPUT[[1]]
            simrv$output$RB <- predicted['RB','mean'] * 100
            simrv$output$RBse <- predicted['RB','se'] * 100
            simrv$output$RSE <- predicted['RSE','mean'] * 100
            simrv$output$RSEse <- predicted['RSE','se'] * 100
            simrv$current <- TRUE
            if (input$updateCFbox) {
                ## CF = simulated / naive RSE value
                CF <- predicted['RSE','mean'] / nrm()$rotRSE
                updateSliderInput(session, "CFslider", value = CF)
            }
        }
        simrv$current <- TRUE

        if (input$simappendbox) addtosummary() 
        
    }
    
    runspacing <- function(sims = FALSE) {
        
        removeNotification("lownr")
        progress <- Progress$new(session, min=1, max=15)
        on.exit(progress$close())
        if (sims) {
            progress$set(message = 'Simulating RSE for each spacing...')
            rotrv$output <- optimalSpacing(
                D = density(),
                traps = detectorarray(),
                detectpar = list(lambda0 = input$lambda0, sigma = input$sigma),
                noccasions = input$noccasions,
                nrepeats = nrepeats(),
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
                D = density(),
                traps = detectorarray(),
                detectpar = list(lambda0 = input$lambda0, sigma = input$sigma),
                noccasions = input$noccasions,
                nrepeats = nrepeats(),
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
    
    addtosummary <- function() {
        ## input$fields is character vector of selected fields
        
        ## tentatively suppress 2019-01-18 
        ## invalidateOutputs()
        df <- data.frame(
            date = format(Sys.time(), "%Y-%m-%d"),
            time = format(Sys.time(), "%H:%M:%S"),
            note = if (simrv$current && input$title=="") paste(input$title, "simulated") else input$title,
            detector = input$detector,
            source = input$arrayinput,
            nx = if (input$arrayinput=="Grid") input$nx else
                if (input$arrayinput=="Line") input$nline else NA,
            ny = if (input$arrayinput=="Grid") input$ny else NA,
            spacex = if (input$arrayinput=="Grid") input$spx else
                if (input$arrayinput=="Line") input$spline else NA,
            spacey = if (input$arrayinput=="Grid") input$spy else NA,
            ndetectors = nrow(detectorarray()) * nrepeats(),
            noccasions = input$noccasions,
            nrepeats = input$nrepeats,
            distribution = input$distributionbtn,
            detectfn = input$detectfn,
            
            D = density(),
            lambda0 = input$lambda0,
            sigma = input$sigma,
            detperHR = nrm()$detperHR,
            k = if (input$detectfn=="HHN") round(density()^0.5 * input$sigma / 100,3) else NA,
            En = round(nrm()$En,1),
            Er = round(nrm()$Er,1),
            Em = round(nrm()$Em,1),
            rotRSE = round(nrm()$rotRSE * 100, 1),
            CF = round(nrm()$CF, 3),
            # perkm = input$perkm,
            # perarry = input$perarray,
            # perdetr = input$perdetector,
            # pervist = input$pervisit,
            # perdetn = input$perdetection,
            
            route = round(arraypathlength()/1000 * nrepeats(),3),
            cost = round(nrm()$totalcost, 2)
        )

        newfields <- ""        
        
        simdf <- data.frame(
            simfn = "",
            nrepl = NA,
            simtime = NA,
            simRSE = NA, 
            simRSEse = NA,
            simRB = NA,
            simRBse = NA)
        
        if (simrv$current && !is.null(simrv$output$fit)) {
            simdf$simfn <- input$packagebtn
            simdf$nrepl <- input$nrepl
            simdf$simtime <- round(simrv$output$proctime,2)
            if (simrv$output$fit) {
                simdf$simRSE <- simrv$output$RSE
                simdf$simRSEse <- simrv$output$RSEse
                simdf$simRB <- simrv$output$RB
                simdf$simRBse <- simrv$output$RBse
            }
            newfields <- unique(c(isolate(input$fields2), c("simfn", "nrepl", "simtime") ))
        }
        
        df <- cbind(df, simdf)
        sumrv$value <- rbind (sumrv$value, df)
        # updateCheckboxGroupInput("fields2", selected = newfields)  ## FAILS 2019-01-23
        rownames(sumrv$value) <- paste0("Scenario", 1:nrow(sumrv$value))
    }
    ##############################################################################
    
    getSPcode <- function (inputfilename, varname, apply = TRUE) {
        filename <- inputfilename[1,1]
        if (is.null(filename) || !apply) {
            return("")
        }
        else {
            ext <- tolower(tools::file_ext(filename))
            if (ext == "txt") {
                code <- paste0( 
                    "# coordinates from text file\n",
                    "coord <- read.table('", filename, "')   # read boundary coordinates\n",
                    varname, " <- secr:::boundarytoSP(coord)  # convert to SpatialPolygons\n")
            }
            else if (ext %in% c("rdata", "rda")) {
                objlist <- load(inputfilename[1,4])
                code <- paste0( 
                    "# SpatialPolygons from RData file\n",
                    "objlist <- load('", filename, "')\n",
                    varname, " <- get(objlist[1]) \n")
            }
            else if (ext == "rds") {
                code <- paste0( 
                    "# SpatialPolygons from RDS file\n",
                    varname, " <- readRDS('", filename, "') \n")
            }
            else {
                code <- paste0(
                    "# ESRI polygon shapefile\n",
                    varname, " <- rgdal::readOGR(dsn = '", 
                    tools::file_path_sans_ext(basename(filename)), 
                    ".shp')\n"
                )
            }
            code
        }
    }    
    
    arraycode <- function (comment = FALSE) {
        # returns the R code needed to generate the specified array, 
        # as a character value
        if (is.null(detectorarray())) {
            code <- ""  
        }
        else {
            if (input$arrayinput == "Grid") {
                code <- paste0(
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
                code <- paste0(
                    "array <- make.grid(",
                    "nx = ", input$nline, ", ",
                    "ny = 1, ",
                    "spacing = ", input$spline, ", \n    ",
                    "detector = '", input$detector, "')\n"
                )
            }
            
            else if (input$arrayinput == "File") {
                
                args <- input$args
                if (args != "")
                    args <- paste0(", ", args)
                code <- paste0("array <- read.traps ('", 
                               input$trapfilename[1,"name"],
                               "', detector = '", input$detector, "'", args, ")\n")
                if (input$scalefactor != 1.0) {
                    code <- paste0(code, 
                                   "# optional scaling about centroid\n",
                                   "meanxy <- apply(array,2,mean)\n",
                                   "array[,1] <- (array[,1]- meanxy[1]) * ", input$scalefactor, " + meanxy[1]\n",
                                   "array[,2] <- (array[,2]- meanxy[2]) * ", input$scalefactor, " + meanxy[2]\n")
                    #"array[,] <- array[,] * ", input$scalefactor, "\n")
                }
            }
            
            else if (input$arrayinput == "Region") {
                
                regioncode <- getSPcode(input$regionfilename, "region", TRUE)
                excludedcode <- getSPcode(input$exclusionfilename, "excluded", input$exclusionbox)
            

                if (input$randomorigin) 
                    origincode <- "NULL"
                else {
                    origincode <- paste0("sp::bbox(region)[,1] + ", input$sppgrid/2)
                }
                
                if (input$chequerboard) { 
                    chequercode <- paste0(",\n    chequerboard = 'white'")
                }
                else {
                    chequercode <- ""  # default
                }
                
                
                if (input$clustertype == "Grid") {
                    clustercode <- paste0("cluster <- make.grid(nx = ", input$nx, ", ny = ", input$ny, 
                                          ", detector = '", input$detector, "', \n",
                                          "    spacex = ", input$spx, ", spacey = ", input$spy, ", ",
                                          "hollow = ", input$hollow, ")\n")
                }
                else if (input$clustertype == "Line") {
                    clustercode <- paste0("cluster <- make.grid(nx = ", input$nline, 
                                          ", ny = 1, detector = '", input$detector, "', \n",
                                          "    spacing = ", input$spline, ")\n")
                }
                else if (input$clustertype == "File") {
                    clustercode <- paste0("cluster <- read.traps ('",
                                          input$trapfilename[1,"name"],
                                          "', detector = '", input$detector, "')\n")
                }
                else {
                    if (input$regiontype == "Systematic")
                        clustercode <- paste0("cluster <- make.grid(nx = 1, ny = 1, detector = '", 
                                              input$detector, "')\n")
                    else
                        clustercode <- ""
                }
                
                if (input$clustertype %in% c("Grid", "Line", "File")) {
                    if (input$chequerboard)
                        edgemethodcode <- paste0(", edgemethod = '", input$edgemethod, "'")
                    else
                        edgemethodcode <- paste0(",\n    edgemethod = '", input$edgemethod, "'")
                    
                    if (input$rotation != 0)
                        rotatecode <- paste0("cluster <- rotate(cluster, ", input$rotation, ")\n")
                    else 
                        rotatecode <- ""
                }
                else {
                    edgemethodcode <- ""
                    rotatecode <- ""
                }
                
                if (excludedcode != "") {
                    exclusioncode <- ", exclude = excluded"
                    if (input$edgemethod == "allinside")
                        exclusioncode <- c(exclusioncode, ", exclmethod = 'alloutside'")
                    else if (input$edgemethod == "anyinside")
                        exclusioncode <- c(exclusioncode, ", exclmethod = 'anyoutside'")
                }
                else
                    exclusioncode <- ""
                
                if ((input$regiontype == "Random" || input$randomorigin) 
                    & input$seedpgrid>0) {
                    seedcode <- paste0("set.seed(", input$seedpgrid, ")\n")
                }
                else {
                    seedcode <- ""
                }
                
                if (input$regiontype == "Systematic") {
                    code <- paste0( 
                        regioncode,
                        excludedcode,
                        clustercode,
                        rotatecode,
                        seedcode,
                        "array <- make.systematic(spacing = ", input$sppgrid, ", ",
                        "region = region, \n", 
                        "    cluster = cluster, origin = ", origincode, chequercode, edgemethodcode, 
                        exclusioncode, ")\n")
                }
                else if (input$regiontype == "Random") {
                    if (input$clustertype %in% c("Grid", "Line", "File"))
                        clusterarg <- ",\n    cluster = cluster"
                    else 
                        clusterarg <- ""
                    
                    code <- paste0( 
                        regioncode,
                        excludedcode,
                        clustercode,
                        rotatecode,
                        seedcode,
                        "array <- trap.builder(n = ", input$numpgrid, ", ",
                        "method = '", input$randomtype, "', ",
                        "region = region", clusterarg, 
                        edgemethodcode, exclusioncode, ")\n")
                }
                else stop ("unknown regiontype")
                
            }
            else stop ("unknown arrayinput")
            
            if (comment) {
                tmp <- lapply(strsplit(code, "\n")[[1]], function(x) paste0("# ", x))
                tmp$sep <- "\n"
                code <- do.call(paste, tmp)
            }
        }
        code        
    }
    ##############################################################################
    
    maskcode <- function (arrayname) {
        type <- if (input$maskshapebtn == 'Rectangular') 'traprect' else 'trapbuffer'
        buffer <- as.character(round(input$habxsigma * input$sigma,2))

        polycode <- ""
        polyhabitat <- ""
        
        if (input$polygonbox && !is.null(input$habpolyfilename)) { 
            polyhabitat <- input$includeexcludebtn == "Include"
            polycode <- getSPcode(input$habpolyfilename, "poly", input$polygonbox)
        }
        paste0(polycode,
               "mask <- make.mask (", arrayname, 
               ", buffer = ", buffer, 
               ", nx = ", input$habnx, 
               ", type = '", type, "'",  
               if (polycode == "") "" else ",\n    poly = poly",
               if (polycode == "") "" else ", poly.habitat = ", polyhabitat,
               ")\n")
    }
    ##############################################################################
    
    nrmcode <- function() {
        pathl <- as.character(arraypathlength()/1000)
        detfn <- input$detectfn
        if (is.character(detfn)) detfn <- paste0("'", detfn, "'")
        costcode <- if (nrm()$totalcost==0) ""
        else paste0(
            ",\n    unitcost = list(perkm = ", input$perkm, ", ",
            "perarray = ", input$perarray, ", ",
            "perdetector = ", input$perdetector, ",\n",
            "                    pervisit = ", input$pervisit, ", ",
            "perdetection = ", input$perdetection, ")",
            ",\n    routelength = ", pathl,  ", setupoccasion = ", input$setupoccasion, ", costing = TRUE")
        paste0(
            "# R code to generate main results\n",
            "library(secrdesign)\n\n",
            "# array type : ", input$arrayinput, "\n",
            arraycode(),
            "\n",
            maskcode("array"),
            "\n",
            "scen <- make.scenarios(trapsindex = 1, noccasions = ", input$noccasions, ", ", 
            "nrepeats = ", nrepeats(), ",\n    D = ", density(), ", sigma = ", input$sigma, ", ",
            "lambda0 = ", input$lambda0, ", detectfn = ", detfn, ")\n\n",
            
            "scensum <- scenarioSummary(scen, trapset = array, mask = mask, CF = ", 
            round(input$CFslider, 3), 
            costcode, ")\n\n",
            "# scensum is a dataframe with one row and columns for En, Er etc.\n",
            "# see ?scenarioSummary for details")
    }
    ##############################################################################
    
    maskOK <- function () {
        if (!is.null(poly())) {
            sum(pointsInPolygon(detectorarray(), poly())) > 0
        }
        else TRUE
    }
    ##############################################################################
    
    
    ## reactive
    
    ## arraypathlength
    ## invalidateOutputs
    ## simarg
    ## array
    ## poly
    ## region
    ## mask
    ## pop
    ## Pxy
    ## nrm
    ## validspacing
    
    ##############################################################################
    
    output$validspacing <- reactive({rotrv$current})   ## for conditionalPanel
    
    arraypathlength <- reactive({
        
        pathl <- NA
        if (input$routetype %in% c("Sequential", "TSP", "Manual")) {
            if (input$routetype=="manual")
                seq <- manualroute$seq
            else
                seq <- attr(detectorarray(), "seq")
            if (!is.null(seq)) {
                #     stop("requires seq attribute or manual route")
                if (length(seq)==1)
                    pathl <- 0
                else {
                    trps <- detectorarray()[seq,]
                    ntrps <- nrow(trps)
                    if (input$returnbox)
                        dxy <- trps - trps[c(2:ntrps, 1),]
                    else
                        dxy <- trps[-1,] - trps[-ntrps,]
                    d <- sqrt(apply(dxy^2,1,sum))
                    pathl <- sum(d)
                }
            }
        }
        else if (input$routetype == "SumSpacing") {
            d <- as.matrix(dist(detectorarray()))
            diag(d) <- NA
            d <- apply(d,1,min, na.rm = TRUE)
            if (length(table(d)) == 1) {
                pathl <- (nrow(detectorarray())-1+ input$returnbox) * d[1]
            }
            else {
                pathl <- sum(d)   
            }
        }
        
        pathl
        
    })
    
    ##############################################################################
    
    invalidateOutputs <- reactive({
        simrv$current <- FALSE
        rotrv$current <- FALSE
        pxyrv$value <- NULL
        updateNumericInput(session, "D", step = 10^trunc(log10(density()/50)))
    })
    
    ##############################################################################
    
    # note change to any of simulation settings
    simarg <- reactive({
        simrv$current <- FALSE
        list(
            nrepl = input$nrepl,
            ncores = input$ncores,
            seed = input$seed,
            simnx = input$habnx,
            simxsigma = input$habxsigma,
            method = input$method,
            fit = input$packagebtn %in% c('openCR.fit', 'secr.fit'))}
    )
    
    ##############################################################################
    
    readtrapfile <- function (scale) {
        inFile <- input$trapfilename
        trps <- NULL
        if (!is.null(inFile)) {
            filename <- input$trapfilename[1,"datapath"]
            if (is.null(filename))
                stop("provide valid filename")
            args <- input$args
            if (args != "")
                args <- paste0(", ", args)
            readtrapscall <- paste0("read.traps (filename, detector = input$detector", args, ")")
            trps <- try(eval(parse(text = readtrapscall)))
            if (scale != 1.0) {
                # trps[,] <- trps[,] * scale
                meanxy <- apply(trps,2,mean)
                trps[,1] <- (trps[,1]- meanxy[1]) * scale + meanxy[1]
                trps[,2] <- (trps[,2]- meanxy[2]) * scale + meanxy[2]
            }
            
            if (!inherits(trps, "traps")) {
                showNotification("invalid trap file or arguments; try again",
                                 type = "warning", id = "badarray", duration = seconds)
            }
        }    
        trps
    }
    
    detectorarray <- reactive(
        {
            simrv$current <- FALSE
            rotrv$current <- FALSE
            pxyrv$value <- NULL
            arrrv$v  ## create dependency on randomarraybtn
            if (!input$autorefresh) return(NULL)
            trps <- NULL
            removeNotification("badarray")
            if (input$arrayinput == "Grid") {
                trps <- make.grid(nx = input$nx, ny = input$ny, detector = input$detector,
                                  spacex = input$spx, spacey = input$spy,
                                  hollow = input$hollow, ID = "numxb")
            }
            else if (input$arrayinput == "Line") {
                trps <- make.grid(nx = input$nline, ny = 1, detector = input$detector,
                                  spacing = input$spline)
            }
            else if (input$arrayinput == 'File') {
                trps <- readtrapfile(input$scalefactor)
            }
            else if (input$arrayinput=='Region') {
                if (input$nrepeats>1) {
                    updateNumericInput(session, "nrepeats", value = 1)
                }
                if (is.null(region())) {
                    trps <- NULL
                }
                else {
                    if (input$randomorigin || input$regiontype == "Random") {
                        if (input$seedpgrid>0) {
                            set.seed(input$seedpgrid)
                        }
                    }
                    if (input$randomorigin) {
                        origin <- NULL
                    }
                    else {
                        origin <- sp::bbox(region())[,1] + input$sppgrid/2
                    }
                    if (input$clustertype == "Grid") {
                        cluster <- make.grid(nx = input$nx, ny = input$ny, detector = input$detector,
                                             spacex = input$spx, spacey = input$spy,
                                             hollow = input$hollow, ID = "numxb")
                    }
                    else if (input$clustertype == "Line") {
                        cluster <- make.grid(nx = input$nline, ny = 1, detector = input$detector,
                                             spacing = input$spline)
                    }
                    else if (input$clustertype == "File") {
                        cluster <- readtrapfile(input$scalefactor)
                    }
                    else {
                        cluster <- make.grid(1, 1, detector = input$detector)
                    }
                    
                    #########################################################
                    ## check expected number
                    regionarea <- polyarea(region())
                    npercluster <- nrow(cluster)
                    if (input$regiontype == "Systematic")
                        expectedndetector <- regionarea/(input$sppgrid/100)^2 * npercluster
                    else
                        expectedndetector <- input$numpgrid * npercluster
                    if (expectedndetector > input$maxdetectors*2) {
                        showNotification("expected N detectors exceeds limit",
                                         type = "error", id = "toomany",
                                         duration = seconds)
                        return(NULL)
                    }
                    #########################################################
                    
                    
                    
                    if (input$clustertype != "Single detector")
                        cluster <- rotate (cluster, input$rotation)
                    
                    if (input$regiontype == "Random") {   # randompoint
                        ntrps <- input$numpgrid
                        if (ntrps > 0)
                            trps <- trap.builder(n = ntrps, 
                                                 method = input$randomtype,
                                                 region = region(),
                                                 cluster = cluster,
                                                 edgemethod = input$edgemethod,
                                                 exclude = exclusion(),
                                                 exclmethod = switch(input$edgemethod, 
                                                                     allinside = "alloutside", 
                                                                     anyinside = "anyoutside", 
                                                                     "clip"))
                    }
                    else {
                        args <- list(spacing = input$sppgrid, 
                                     cluster = cluster, 
                                     region = region(),
                                     origin = origin,
                                     edgemethod = input$edgemethod,
                                     exclude = exclusion(),
                                     exclmethod = switch(input$edgemethod, 
                                                         allinside = "alloutside", 
                                                         anyinside = "anyoutside", 
                                                         "clip"))
                        
                        if (input$chequerboard)
                            args$chequerboard <- "white"
                        
                        trps <- do.call(make.systematic, args)
                    }
                }
                
            }
            else stop ("unrecognised array input")

            if (!is.null(trps)) {
                attr(trps, "arrayspan") <- suppressWarnings(pmax(0, max(dist(trps))))
            }
            if (!is.null(trps) && (nrow(trps) > input$maxdetectors)) {
                showNotification(paste0("more than ", input$maxdetectors, " detectors; try again"),
                                 type = "warning", id = "bigarray", duration = seconds)
                trps <- NULL
            }
            
            if (!is.null(trps) && (nrow(trps) == 0)) {
                showNotification(paste0("no detectors; try again"),
                                 type = "warning", id = "zeroarray", duration = seconds)
                trps <- NULL
            }
            
            if (is.null(trps) || input$arrayinput == "Region") {
                hideTab(inputId = "navlist", target = "Spacing")
            }
            else {
                showTab(inputId = "navlist", target = "Spacing")
            }
            
            if (is.null(trps)) {
                hideTab(inputId = "navlist", target = "Costing")
                hideTab(inputId = "navlist", target = "Simulation")
                return (NULL)
            }
            else {
                showTab(inputId = "navlist", target = "Costing")
                showTab(inputId = "navlist", target = "Simulation")
                addsequence (trps, input$routetype)
            }
        }
    )
    ##############################################################################
    
    poly <- reactive( {
        if (input$polygonbox) {
            readpolygon(input$habpolyfilename)
        }
        else {
            NULL
        }
    }
    )
    ##############################################################################
    
    region <- reactive( { 
        if (input$arrayinput == 'Region') {
            readpolygon(input$regionfilename) 
        }
        else {
            NULL
        }
    }
    )
    
    ##############################################################################
    exclusion <- reactive( { 
        if (!is.null(input$exclusionfilename) && input$exclusionbox) {
            readpolygon(input$exclusionfilename) 
        }
        else {
            NULL
        }
    }
    )
    ##############################################################################
    
    mask <- reactive( {
        rotrv$current <- FALSE
        pxyrv$value <- NULL
        if (!maskOK()) showNotification("no detectors in habitat polygon(s)",
                                        type = "warning", id = "notrapsinpoly",
                                        duration = seconds)
        msk <- make.mask (detectorarray(),
                          buffer = input$habxsigma * input$sigma,
                          nx = input$habnx,
                          type = if (input$maskshapebtn=='Rectangular') 'traprect' else 'trapbuffer',
                          poly = poly(),
                          poly.habitat = input$includeexcludebtn == "Include",
                          keep.poly = FALSE)
        msk
    }
    )
    ##############################################################################
    
    pop <- reactive(
        {
            poprv$v
            core <- detectorarray()
            if (is.null(core) || (input$D == 0)) {
                return (NULL)
            }
            if (density() * maskarea(mask()) > 10000) {
                showNotification("population exceeds 10000; try again",
                                 type = "error", id = "bigpop", duration = seconds)
                return(NULL)
            }
            else removeNotification("bigpop")
            Ndist <- if (input$distributionbtn == 'Poisson') 'poisson' else 'fixed'
            if (input$onlymaskbox) {
                if (nrow(mask())==0) {
                    showNotification("incompatible mask",
                                     type = "error", id = "badmask", duration = seconds)
                    pop <- NULL
                }
                else {
                    pop <- sim.popn (D = density(), core=mask(), model2D="IHP", Ndist = Ndist)
                }
            }
            else { # rectangular area
                pop <- sim.popn (D = density(), core=core, Ndist = Ndist,
                                 buffer = input$habxsigma * input$sigma)
            }
            pop
        }
    )
    ##############################################################################
    
    Pxy <- reactive({
        # DOES NOT USE poly()
        invalidateOutputs()
        trps <- detectorarray()
        # msk <- make.mask(trps, buffer = input$sigma * input$pxyborder, nx = input$pxynx)
        msk <- make.mask(trps, buffer = border(input$pxyborder), nx = input$pxynx)
        Pxy <- pdot(msk, trps, detectfn = input$detectfn,
                    detectpar=list(lambda0=input$lambda0, sigma = input$sigma),
                    noccasions = input$noccasions)
        sumPxy <- sum(Pxy)
        EPxy <- sum(Pxy^2) / sumPxy
        EPxy2 <- sum(Pxy^3) /sumPxy
        varPxy <- EPxy2 - EPxy^2
        sinuosity <- if (nrow(trps)<=1) NA else attr(trps, "arrayspan") / (spacing(trps) * (nrow(trps)-1))
        list(CVPxy = sqrt(varPxy)/EPxy, sinuosity = sinuosity, esa = sumPxy * attr(msk, "area"))
    })
    ##############################################################################
    
    nrm <- reactive({
        trps <- detectorarray()
        if (is.null(trps)) return (NULL)
        # appears not to be needed 2019-01-19
        # invalidateOutputs()
        
        msk <- mask()
        if (nrow(msk)>0) {
            pathl <- arraypathlength()
            scen <- make.scenarios(trapsindex = 1, noccasions = input$noccasions, 
                                   nrepeats = nrepeats(), D = density(), sigma = input$sigma, 
                                   lambda0 = input$lambda0, detectfn = input$detectfn)
            scensum <- scenarioSummary(scen,
                                       trapset = trps,
                                       mask = msk,
                                       CF = input$CFslider,  
                                       routelength = pathl / 1000,
                                       costing = TRUE,
                                       setupoccasion = input$setupoccasion,
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
            if (!is.finite(RSE)) RSE <- maxRSE
            updateSliderInput(session, "RSEslider",
                              min = 1.0,
                              max = maxRSE,
                              value = RSE,
                              step = 0.1)
            
            # nr
            scensum
        }
        else {
            showNotification("invalid mask; check sigma parameter", type = "warning", id = "invalidmask",
                             duration = seconds)
            
            NULL
        }
    })
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
    poprv <- reactiveValues(v = 0)  # used to invalidate and re-plot popn
    arrrv <- reactiveValues(v = 0)  # used to invalidate and re-plot detectorarray
    manualroute <- reactiveValues(seq = NULL)
    current <- reactiveValues(unit = "ha")
    selecting <- reactiveValues(v=FALSE)
    sumrv <- reactiveValues(
        value = read.csv(text = paste(summaryfields, collapse = ", "))
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
    # suggestlinebtn
    # resetbtn
    # CFslider
    # spacingbtn
    # simulatebtn, simulatebtn2
    # okbtn
    # randompopbtn
    # randomarraybtn
    # routebtn
    # clearallbtn
    # clearlastbtn
    # hollow
    # arrayinput
    # click
    # arrayclick
    # pxyclick
    # appendbtn
    # nrepeats
    # maxupload
    # areaunit
    # chequerboard
    # simappendbox
    
    ##############################################################################
    
    observeEvent(input$simappendbox, ignoreInit = TRUE, {
        if (input$simappendbox) {
            sim <- c(input$fields2, simfields)
            updateCheckboxGroupInput(session, "fields2", selected = sim)
        }
        else {
            nosim <- input$fields2[!(input$fields2 %in% simfields)]
            updateCheckboxGroupInput(session, "fields2", selected = nosim)
        }
    })
    
    observeEvent(input$nrepeats, {
        if (input$arrayinput=='Region' & input$nrepeats>1)
            updateNumericInput(session, "nrepeats", value = 1)
    })
    
    observeEvent(input$suggestbtn, {
        ## Grid
        ## E[n] == E[r]
        if (!input$autorefresh) {
            showNotification("enable auto refresh",
                             type = "error", id = "nosuggest", duration = seconds)
        }
        else {
            optimalspacing <- n.eq.r()
            if (!is.na(optimalspacing)) {
                updateNumericInput(session, "spy", value = optimalspacing)
                updateNumericInput(session, "spx", value = optimalspacing)
            }
        }
    })
    ##############################################################################
    
    observeEvent(input$suggestlinebtn, {
        ## Line
        ## E[n] == E[r]
        if (!input$autorefresh) {
            showNotification("enable auto refresh",
                             type = "error", id = "nosuggestline", duration = seconds)
        }
        else {
            optimalspacing <- n.eq.r()
            if (!is.na(optimalspacing)) {
                updateNumericInput(session, "spline", value = optimalspacing)
            }
        }
    })
    
    ##############################################################################
    
    observeEvent(input$suggestfilebtn, {
        ## Grid
        ## E[n] == E[r]
        if (!input$autorefresh) {
            showNotification("enable auto refresh",
                             type = "error", id = "nosuggestfile", duration = seconds)
        }
        else {
            optimalfactor <- n.eq.r()
            if (!is.na(optimalfactor)) {
                updateNumericInput(session, "scalefactor", value = round(optimalfactor,2))
            }
        }
    })
    ##############################################################################
    
    observeEvent(input$resetbtn, {
        
        ## DOES NOT RESET FILE INPUTS
        ## SEE E.G. https://groups.google.com/forum/#!topic/shiny-discuss/HbTa4v612FA

        current$unit <- "ha"
        
        ## array

        ## grid
        updateSelectInput(session, "detector", selected = "proximity")
        updateTabsetPanel(session, "arrayinput", selected = "Grid")
        updateNumericInput(session, "ny", value = 8)
        updateNumericInput(session, "nx", value = 8)
        updateNumericInput(session, "spy", value = 20)
        updateNumericInput(session, "spx", value = 20)
        updateCheckboxInput(session, "hollow", value = FALSE )

        ## line
        updateNumericInput(session, "nline", value = 20)
        updateNumericInput(session, "spline", value = 20)

        ## region
        updateTabsetPanel(session, inputId = "regiontype", selected = "Random")
        updateCheckboxInput(session, "sppgrid", value = 200 )
        updateCheckboxInput(session, "clustertype", value = "Single detector" )
        updateNumericInput(session, "rotation", value = 0)
        updateCheckboxInput(session, "randomorigin", value = FALSE )
        updateRadioButtons(session, "randomtype", selected = "SRS")
        updateNumericInput(session, "numpgrid", value = 20)
        updateNumericInput(session, "seedpgrid", value = 0)

        ## file
        updateTextInput(session, "args", 
                        value = "", placeholder = "e.g., skip = 1, sep = ','")
        updateNumericInput(session, "scalefactor", value = 1.0)

        ## parameters
        updateNumericInput(session, "D", "D (animals / ha)", value = 5)
        updateSelectInput(session, "detectfn", selected = "HHN")
        updateNumericInput(session, "lambda0", value = 0.2)
        updateNumericInput(session, "sigma", value = 25)

        ## general
        updateTextInput(session, "title", "", value = "",
                        placeholder = "scenario label for Summary")
        updateNumericInput(session, "noccasions", value = 5)
        updateNumericInput(session, "nrepeats", value = 1)
        updateTabsetPanel(session, "tabs", selected = "Array")
        updateRadioButtons(session, "distributionbtn", selected = "Poisson")
        updateCheckboxInput(session, "autorefresh", value = TRUE)
        # updateCheckboxInput(session, "autoappend", value = FALSE)

        ## pop plot
        updateCheckboxInput(session, "showHRbox", "Display 95% home range", value = FALSE)
        updateCheckboxInput(session, "showmaskbox", "Display mask", value = FALSE)
        updateCheckboxInput(session, "onlymaskbox", "Restrict to mask", value = TRUE)

        ## power plot
        updateCheckboxInput(session, "adjustRSEbox", value = TRUE)
        updateCheckboxInput(session, "powertype", "95% CI", value = TRUE)
        updateNumericInput(session, "xpos", value = 0)

        ## costing
        updateNumericInput(session, "perkm", value = 0)
        updateNumericInput(session, "perarray", value = 0)
        updateNumericInput(session, "perdetector", value = 0)
        updateNumericInput(session, "perdetection", value = 0)
        updateNumericInput(session, "pervisit", value = 0)
        updateRadioButtons(session, "routetype", selected = "Sequential")
        updateCheckboxInput(session, "returnbox", value = FALSE)
        updateCheckboxInput(session, "setupoccasion", value = TRUE)

        ## spacing
        updateTabsetPanel(session, "spacingtabs", selected = "RSE")
        updateCheckboxInput(session, "spacingsimbox", value = FALSE)

        ## simulate
        updateNumericInput(session, "nrepl", value = 5)
        updateNumericInput(session, "ncores", value = 1)
        updateNumericInput(session, "seed", value = 0)
        #updateNumericInput(session, "simnx", value = 32)
        updateSelectInput(session, "method", selected = "none")
        updateRadioButtons(session, "packagebtn", selected = "openCR.fit")
        updateCheckboxInput(session, "simappendbox", value = TRUE)

        updateCheckboxGroupInput(session, "fields1", selected = summaryfields[fieldgroup1])
        updateCheckboxGroupInput(session, "fields2", selected = summaryfields[fieldgroup2])

        ## options
        # updateTextInput(session, "savefilename", value = "log.txt")
        # updateCheckboxInput(session, "appendbox", value = TRUE)

        ## detector array
        updateCheckboxInput(session, "lockxy", value = TRUE)
        updateCheckboxInput(session, "randomorigin", value = FALSE)
        updateRadioButtons(session, "edgemethod", selected = "clip")
        updateNumericInput(session, "maxupload", value = 5)
        updateRadioButtons(session, "areaunit", selected = "ha")

        updateRadioButtons(session, "currency", selected = "$")

        ## habitat
        updateNumericInput(session, "habxsigma", value = 4)
        updateNumericInput(session, "habnx", value = 32)
        updateRadioButtons(session, "maskshapebtn", selected = "Rounded")
        updateCheckboxInput(session, "polygonbox", value = TRUE)
        updateCheckboxInput(session, "exclusionbox", value = TRUE)
        updateRadioButtons(session, "includeexcludebtn", selected = "Include")

        ## array plot
        updateCheckboxInput(session, "entireregionbox", value = TRUE)
        updateCheckboxInput(session, "snaptodetector", value = FALSE)
        updateRadioButtons(session, "gridlines", selected = "None")

        ## pxy plot
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
        
        invalidateOutputs()
    })
    
    ##############################################################################
    
    observeEvent(input$CFslider, {
        rotrv$current <- FALSE
    })
    ##############################################################################
    
    observeEvent(input$maxupload, {
        options(shiny.maxRequestSize = input$maxupload*1024^2)
    })
    ##############################################################################
    
    observeEvent(input$areaunit, ignoreInit = TRUE, {
        new.unit <- isolate(input$areaunit)
         if (new.unit != current$unit) {
            if (new.unit=="ha") {
                newD <- isolate(input$D)/100
            }
            else {
                newD <- isolate(input$D)*100
            }
            updateNumericInput(session, "D", paste0("D (animals / ", new.unit, ")"), value = newD)
            current$unit <- new.unit
        }
    })

    observeEvent(input$alpha, {
        updateCheckboxInput(session, "powertype", label = paste0(
            round(100 *(1-input$alpha), 1), "% CI"))
    })
    
    observeEvent(input$chequerboard, {
        if (input$chequerboard && compareVersion(as.character(secrversion), '3.2.0') < 0) {
            showNotification("chequerboard requires secr version >= 3.2.0",
                             type = "error", id = "oldsecr", duration = seconds)
            updateCheckboxInput(session, "chequerboard", value = FALSE)
        }
    })
    
    observeEvent(input$spacingbtn, {
        if (input$spacingsimbox) {
        
            ## time check 2018-12-05
            methodfactor <- 1 + ((input$method != "none") * 4)
            functionfactor <- 1 + ((input$packagebtn != "openCR.fit") * 3)
            detectorfactor <- switch(input$detector, proximity = 1, multi = 0.6, count = 4)
            time <- nrow(mask()) * nrow(detectorarray()) / 1e9 * ## blocked 2019-01-14 nrepeats() * 
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
    
    observeEvent(c(input$simulatebtn, input$simulatebtn2), ignoreInit = TRUE, {
        ## ignoreInit blocks initial execution when simulatebtn2 goes from NULL to 0
        if (!is.null(detectorarray())) {
            removeNotification("lownr")
            methodfactor <- 1 + ((input$method != "none") * 4)
            functionfactor <- switch(input$packagebtn, secr.fit = 4, openCR.fit = 1, 0.1)
            detectorfactor <- switch(input$detector, proximity = 1, single = 0.6, multi = 0.6, count = 4)
            En <- nrm()$En
            if (is.na(En)) En <- 100  ## surrogate for 'single' detectors
            time <- nrow(mask()) * nrow(detectorarray()) / 4.5e9 * ## blocked 2019-01-14 nrepeats() * 
                En * input$noccasions * input$nrepl * 
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
    
    observeEvent(input$randompopbtn, ignoreInit = TRUE, {
        # invalidates pop when button pressed
        poprv$v <- poprv$v + 1
    })
    
    ##############################################################################
    observeEvent(input$randomarraybtn, ignoreInit = TRUE, {
        # invalidates detectorarray when button pressed
        arrrv$v <- arrrv$v + 1
    })
    
    ##############################################################################
    
    observeEvent(input$routebtn, {
        manualroute$seq <- NULL
    }   )
    
    ##############################################################################
    
    observeEvent(input$selectfieldsbtn, {
        selecting$v <- ! selecting$v
        output$selectingfields <- renderText(selecting$v)
            
    }   )
    
    observeEvent(input$selectallbtn, {
        updateCheckboxGroupInput(session, "fields1", selected = summaryfields[fieldgroup1])
        updateCheckboxGroupInput(session, "fields2", selected = summaryfields[fieldgroup2])
    }   )
    
    observeEvent(input$selectnonebtn, {
        updateCheckboxGroupInput(session, "fields1", selected = "")
        updateCheckboxGroupInput(session, "fields2", selected = "")
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
    
    observeEvent(c(input$arrayinput, input$resetCFbtn), {
        if (input$arrayinput != "Region") {
            removeNotification(id = "nofile")
            removeNotification(id = "norgdal")
        }
        if (input$arrayinput=="Line" || 
            (input$arrayinput=="Region" & input$clustertype=="Line")) 
            CF <- 1.3
        else if ((input$arrayinput=="Grid" & input$hollow) ||
                 (input$arrayinput=="Region" & input$clustertype=="Grid" & input$hollow))
            CF <- 1.2
        else 
            CF <- 1.0
        updateSliderInput(session, "CFslider", value = CF)
    })
    
    ##############################################################################
    
    observeEvent(input$click, {
        xy <- c(input$click$x, input$click$y)
        trp <- nearesttrap(xy, detectorarray())
        manualroute$seq <- c(manualroute$seq, trp)
    })
    
    ##############################################################################
    
    observeEvent(input$pxyclick, {
        invalidateOutputs()
        trps <- detectorarray()
        
        border <- border(input$pxyborder)
        
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
    
    observeEvent(input$CIclick, {
        invalidateOutputs()
        if (input$powertype) {
            updateNumericInput(session, "xpos", value = round(input$CIclick$x))
        }
        else {
        }
    })
    
    observeEvent(input$currency, {
        updateNumericInput(session, "perkm", label = paste("Travel per km", input$currency))
        updateNumericInput(session, "perarray", label = paste("Cost per array", input$currency))
        updateNumericInput(session, "perdetector", label = paste("Cost per detector", input$currency))
        updateNumericInput(session, "pervisit", label = paste("Cost per detector visit", input$currency))
        updateNumericInput(session, "perdetection", label = paste("Cost per detection", input$currency))
    })
    
    ##############################################################################
    
    observeEvent(input$appendbtn, {
        if (!is.null(detectorarray()))
            addtosummary()
    })
    
    ##############################################################################

    # observeEvent(input$summarybtn, {
    #     ap <- isolate(input$appendbox)
    #     filename <- isolate(input$savefilename)
    #     ex <- file.exists(filename)
    #     write.table(sumrv$value[c(input$fields1, input$fields2),],
    #                 append = ap & ex,
    #                 file = filename,
    #                 col.names = !ap | !ex,
    #                 row.names = FALSE,
    #                 quote = FALSE)
    # }
    # )
    # ##############################################################################
    # 
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
        if (is.null(detectorarray())) "" # abort if no valid array
        else {
            
            ## inp <- oS2()
            invalidateOutputs()
            
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
                
                arraycode(), "\n",
                
                "oS <- optimalSpacing(D = ", density(), ", traps = array,\n",
                "    detectpar = list(lambda0 = ", input$lambda0, ", sigma = ", input$sigma, "),\n",
                "    detectfn = '", input$detectfn, "', noccasions = ", input$noccasions, ", nrepeats = ", nrepeats(), ",\n",
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
        if (is.null(detectorarray())) {
            ""
        }
        else {
            D  <- density() * nrepeats()
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
            fit <- input$packagebtn %in% c("openCR.fit", "secr.fit")
            countcode <- if (fit) {
                    if (compareVersion(as.character(secrdesignversion), '2.5.7') < 0) "" else "summary(count(sims))$OUTPUT[[1]]\n" 
                } else
                paste0(
                    "sumc <- function(x) {\n",
                    "    c(n = sum(!is.na(x)),\n",
                    "     mean = mean(x, na.rm = TRUE),\n",
                    "     se =  sd(x, na.rm = TRUE)/ sum(!is.na(x))^0.5)}\n",
                    "t(apply(sims$output[[1]], 2, sumc))\n")
            
            fitcode <- if (fit)
                paste0("output <- summary(predict(sims))$OUTPUT[[1]]\n",
                       "c(sims$proctime,\n",
                       RBcode,
                       "  RSE = output['RSE','mean'] * 100,\n",
                       "  RSEse = output['RSE','se'] * 100\n)")
            else ""

            paste0(
                "library(secrdesign)\n\n",
                
                "# construct detector array and mask\n",
                arraycode(), 
                maskcode("array"), "\n",
                
                "# simulate\n",
                "scen <- make.scenarios(",
                "noccasions = ", input$noccasions, ", ",
                "nrepeats = ", nrepeats(), ",\n    ",
                "D = ", density(), ", ",
                "sigma = ", input$sigma, ", ",
                "lambda0 = ", input$lambda0, ", ",
                "detectfn = '", input$detectfn, "')\n",
                
                "sims <- run.scenarios (",
                "nrepl = ", input$nrepl, ", ",
                "scenarios = scen, \n",
                "    trapset = array, maskset = mask,",
                " pop.args = list(Ndist = '", Ndist, "'),\n",
                
                "    fit = ", fit, 
                ", extractfn = summary, ",
                if (fit) paste0("fit.function = '", input$packagebtn, "', \n",
                                "    fit.args = list(detectfn = '", input$detectfn, "', ",
                                "method = '", input$method, "',\n", 
                                "        ", distncode, "),\n")
                else paste0("terse = TRUE, moves = true, \n"),
                
                "    ncores = ", input$ncores, ", ",
                "byscenario = FALSE, seed = ", if (input$seed==0) "NULL" else input$seed, 
                ")\n\n",
                
                "# return selected results\n",
                countcode,
                fitcode
            )                
        }
    })
    ##############################################################################
    
    output$ntrapPrint <- renderText({
        gr <- detectorarray()
        glength <- attr(gr, "arrayspan")
        if (!is.null(gr)) {
            if (input$arrayinput=='Region' & input$clustertype %in% c("Grid", "Line")) {
                if (nrow(gr)==0)
                    ncluster <- 0
                else
                    ncluster <- length(unique(clusterID(gr)))
                clustertext <- paste0( " in ", ncluster, " clusters")
                cr <- "\n"
            }
            else {
                clustertext <- cr <- ""
            }
            if (glength/input$sigma > 100) 
                ratio <- round(glength/input$sigma)
            else 
                ratio <- round(glength/input$sigma,1)
            paste0(nrow(gr), " ", input$detector, " detectors", clustertext, 
                   "; ", cr, "diameter ", lengthstr(glength), " (", ratio, " sigma)")
        }
        else ""
    })
    ##############################################################################
    
    output$nrmPrint <- renderText({
        progress <- Progress$new(session, min = 1, max = 15)
        on.exit(progress$close())
        progress$set(message = 'Refreshing...',
                     detail = '')
        nrmval <- nrm()
        if (is.null(nrmval)) return (NULL)
        star <- if (nrepeats()>1) "*" else ""
        nrepeatstr <- if (nrepeats()>1) paste0("\n* ", nrepeats(), " arrays") else ""
        Pxyval <- Pxy()
        kstr <- if (input$detectfn=="HHN") paste0(
            "Overlap coefficient k = ",
            round(density()^0.5 * input$sigma / 100,3), '\n')
        else ""
        
        coststr <- if (is.null(nrmval$totalcost) || is.na(nrmval$totalcost) || (nrmval$totalcost<=0))
            ""
        else
            paste0( "\nTotal cost = ", input$currency, sprintf("%.2f", nrmval$totalcost), star)
        
        if (attr(detectorarray(), "arrayspan") < (5 * input$sigma)) {
            removeNotification("lownr")
            showNotification("Pathological design - array span < 5.sigma",
                             type = "warning", id = "zeronm", duration = NULL)
        }
        if (!any(is.na(c(nrmval$En, nrmval$Er, nrmval$Em)))) {
            if (nrmval$Em<5) {
                removeNotification("lownr")
                showNotification("Pathological design - E(m) less than 5",
                                 type = "warning", id = "zeronm", duration = NULL)
            }
            else {
                removeNotification("zeronm")
                if (nrmval$En<20 | nrmval$Er<20) {
                    showNotification("Low E(n) or E(r) - simulate to check RSE",
                                     type = "warning", id = "lownr", duration = NULL)
                }
                else {
                    removeNotification("lownr")
                }
            }
        }
        paste0(
            
            "Expected number of individuals detected n = ", 
            round(nrmval$En,1), star,'\n',
            "Expected number of recaptures r = ",
            round(nrmval$Er,1), star, '\n',
            "Expected number of movement recaptures m = ",
            round(nrmval$Em,1), star,'\n',
            "Median detectors per 95% home range = ", 
            nrmval$detperHR, '\n',
            kstr,
            "Effective sampling area = ",
            areastr(nrmval$esa), star, " (mask ", areastr(nrmval$maskarea * nrepeats()), star, ")\n",
            "Rule-of-thumb RSE = ",
            round(nrmval$rotRSE * 100, 1), "%", star, " (correction factor ", round(input$CFslider,3), ")",
            ## "\nRule-of-thumb RSE (binomial) = ",
            ## round(nrmval$rotRSEbin * 100, 1), "% (correction factor ", round(input$CFslider,3), ")",
            coststr, nrepeatstr
        )
    })
    ##############################################################################
    
    output$costPrint <- renderText({
        if (!is.null(detectorarray())) {
            costs <- nrm()
            if (is.null(costs)) {
                showNotification("costing failed; check parameters",
                                 type = "error", id = "nocost", duration = seconds)
                return("")
            }
            else {
                nocc1 <- input$noccasions + as.numeric(input$setupoccasion)
                paste0(
                    "  Travel      ", input$currency, " ",
                    round(costs$travel,2), "  (", round(arraypathlength()/1000  * 
                                                            nocc1 * nrepeats(),3), ' km)\n',
                    "  Arrays      ", input$currency, " ",
                    round(costs$arrays,2), "  (", nrepeats(), ") \n",
                    "  Detectors   ", input$currency, " ",
                    round(costs$detectors,2), "  (", nrow(detectorarray()) * nrepeats(), ") \n",
                    "  Visits      ", input$currency, " ",
                    round(costs$visits,2), "  (", nrow(detectorarray()) * nocc1 * nrepeats() , ") \n",
                    "  Detections  ", input$currency, " ",
                    round(costs$detections,2), "  (", round(costs$En+costs$Er,1), ')\n\n',
                    "  Total       ", input$currency, " ",
                    round(costs$totalcost, 2)
                )
            }
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
                   round(temp$rotRSE$minimum.RSE*100, 1), " %  (correction factor ", 
                   round(input$CFslider,3), ")")
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
        if (simrv$current) {
            sims <- simrv$output
            out <- paste0(
                
                "Number of replicates = ",
                sims$nrepl, "\n",
                
                "Time for simulations = ",
                round(sims$proctime,2), " seconds",  "\n",
                
                "Number of animals (n) = ",
                round(sims$n,2), " (SE ", round(sims$n.se, 2), ")\n",

                "Number of detections (n+r) = ",
                round(sims$ndet,2), " (SE ", round(sims$ndet.se, 2), ")\n",

                "Number of recaptures (r) = ",
                round(sims$ndet-sims$n,2), "\n",

                "Number of moves (m) = ",
                round(sims$nmov,2), " (SE ", round(sims$nmov.se, 2), ")\n")
            
            if (sims$fit) {
                out <- paste0(out,
                              "Simulated RSE = ",
                              round(sims$RSE, 2), "%", " (SE ",  round(sims$RSEse, 2), "%)", "\n")
                
                if (sims$method != "none") {
                    out <- paste0(out,
                                  "Simulated RB = ",
                                  preplus(round(sims$RB, 2)), "%", " (SE ",  round(sims$RBse, 2), "%)")
                }
            }
            out
        }
        else
            NULL
    })
    ##############################################################################
    
    ## renderPlot
    
    ## arrayPlot
    ## routePlot
    ## detnPlot
    ## popPlot
    ## pxyPlot
    ## powerPlot
    ## RSEPlot
    ## nrmPlot
    ## costPlot
    
    ##############################################################################
    
    output$arrayPlot <- renderPlot( height = 340, width = 340, {
        tmpgrid <- detectorarray()
        if (is.null(tmpgrid)) return (NULL)
        removeNotification("reloadtraps")
        par(mar = c(1,1,1,1), xpd = TRUE)
        if (input$arrayinput=='Region') {
            if (input$entireregionbox)
                sp::plot(region())
            else 
                plot (tmpgrid, gridlines = (input$gridlines != "None"), gridspace = as.numeric(input$gridlines))
            if (!is.null(exclusion()))
                sp::plot(exclusion(), add = TRUE, col = 'lightblue', border = 'lightblue')
            sp::plot(region(), add = TRUE)
            plot (tmpgrid, add = TRUE,  gridlines = (input$gridlines != "None"), gridspace = as.numeric(input$gridlines))
        }
        else {
            plot (tmpgrid, border = border(1), bty='o', xaxs = 'i', yaxs = 'i',
                   gridlines = (input$gridlines != "None"), gridspace = as.numeric(input$gridlines))
        }
    })
    ##############################################################################

    output$trafficlightPlot <- renderPlot( height = 60, width = 20, {
        if (!is.null(nrm())) {
            smallarray <- (attr(detectorarray(), "arrayspan") / input$sigma) < 4.9
            smallarray <- smallarray || nrm()$Em < 5
            RSE <- nrm()$rotRSE
            par(mar=c(0,0,0,0))
            #cols <- c('green','orange','red') 
            cols <- colors()[c(48, 653, 552)]
            # cols <- c('#2dc937', '#e7b416', '#cc3232')
            colour <- 1
            if (RSE>0.20 || smallarray) colour <- 3
            else if (RSE>0.15) colour <- 2
            rect(0,0,20,60, col= grey(0.6), border = NA)
            symbols(x = rep(0.50,3), y = 0.2 + (0:2) * 0.32, circles= rep(0.4,3), fg = grey(0.93), 
                    bg = grey(0.93), inches = FALSE, add = TRUE)
            symbols(x = 0.50, y = 0.2 + (colour-1)*0.32, circles= 0.4, fg = cols[colour], 
                    bg = cols[colour], inches = FALSE, add = TRUE)
            
            
        }
    })
    
    output$routePlot <- renderPlot( height = 320, width = 300, {
        tmpgrid <- detectorarray()
        if (is.null(tmpgrid)) return (NULL)
        pathl <- arraypathlength()
        seq <- attr(tmpgrid, "seq")
        if (input$returnbox) seq <- c(seq,1)
        par(mar=c(3,0,0,2))
        plot (tmpgrid, border = border(1), gridlines = FALSE, bty='o', xaxs='i', yaxs='i')
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
        
        if (input$lambda0 <= 0.7) 
            p <- seq(0,1,0.05)
        else 
            p <- seq(0,1,0.1)
        
        axis(4, at = -log(1 - p), label = p, xpd = FALSE, las = 1)
        mtext(side = 4, line = 3.7, "Detection probability")
    })
    ##############################################################################
    
    output$popPlot <- renderPlot( height = 300, width = 380, {
        core <- detectorarray()
        if (is.null(core)) return (NULL)
        border <- input$habxsigma * input$sigma  # consistent with mask()
        tmppop <- pop()
        n <- if (is.null(tmppop)) 0 else nrow(tmppop)
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
            plot (mask(), add = TRUE, col = grey(0.9), dots=F)
            if (n>0) plot(tmppop, add = TRUE, pch = 16, cex = 0.7, xpd = TRUE, frame = FALSE)
            plot (core, add = TRUE)
        }
        else {
            plot (core, border = border, gridlines = FALSE)
            if (n>0) plot(tmppop, pch = 16, cex = 0.7, xpd = TRUE, add = TRUE, frame = FALSE)
            plot (core, add = TRUE)
            bbox <- sweep(apply(core, 2, range), MAR=1, STATS = c(-border,+border), FUN="+")
            bbox <- expand.grid(data.frame(bbox))[c(1,2,4,3),]
            polygon(bbox)
        }
        if (input$showHRbox & (n>0)) {
            # rad <- rep(2.45 * tmpsig, nrow(tmppop))
            rad <- secr::circular.r(p = 0.95, detectfn = input$detectfn, sigma = tmpsig)
            symbols(tmppop$x, tmppop$y, circles = rep(rad, n),
                    inches = FALSE, fg = grey(0.7), add = TRUE, xpd = FALSE)
        }
    })
    ##############################################################################
    
    border <- function (multiple) {
        spc <- spacing(detectorarray()) 
        if (is.null(spc) || is.na(spc)) spc <- input$sigma
        multiple * spc
    }
    
    output$pxyPlot <- renderPlot( height = 300, width = 380, {
        core <- detectorarray()
        if (is.null(core)) return (NULL)
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
        
        border <- border(input$pxyborder)
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
                     poly.habitat = input$includeexcludebtn == "Include",
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
            # strip.legend("right", legend = seq(0,1,0.1), title = "p.(x)", xpd = TRUE,
            #              legendtype='breaks', inset = 0.01, col = cols[3:12])
            strip.legend("right", legend = c(0,lev[1:10],1), title = "p.(x)", xpd = TRUE,
                         legendtype='breaks', inset = 0.01, col = cols[1:11])
            
        }
    })
    ##############################################################################
    
    output$powerPlot <- renderPlot( height = 320, width = 360, {
        RSE <- input$RSEslider/100
        if (input$powertype) {    ## confidence interval
            par(mar=c(4,4,2,2), mgp=c(2.4,0.7,0))
            headroom <- (input$maxEffect-input$minEffect)/4
            powLU <- plotpowerCI(RSE = RSE, effectRange=c(input$minEffect, input$maxEffect),
                                 estimatedRange = c(input$minEffect, input$maxEffect+headroom),
                                 adjustRSE = input$adjustRSEbox, alpha = input$alpha)
            x <- input$xpos 
            y1 <- approx(x = as.numeric(dimnames(powLU$limits)[[1]]), y = powLU$limits[,1,1], xout = x)$y*100-100
            y2 <- approx(x = as.numeric(dimnames(powLU$limits)[[1]]), y = powLU$limits[,2,1], xout = x)$y*100-100
            segments(x, y1, x, y2, lwd= linewidth, col = "blue")
            text(rep(x,2)+5, c(y1+1, min(y2+1, par()$usr[4]*1.06)), round(c(y1, y2)), adj = 0, cex = 0.9, col = "blue", xpd = TRUE)
        }
        else {
            par(mar=c(4,4,3,1))
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
                                ylab = paste("Cost", input$currency),
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
    
    output$summarytable <- renderTable({
        fields <- c(input$fields1, input$fields2)
        tmp <- t(sumrv$value[,fields])
        if (ncol(tmp)>0) colnames(tmp) <- paste0('Scenario', 1:ncol(tmp))
        tmp <- cbind(Field = fields, tmp)
        tmp } , spacing = "xs"
    )
    
    ##############################################################################
    
    ## downloadhandler
    
    ##############################################################################
    
    output$downloadSummary <- downloadHandler(
        filename = "summary.csv",
        content = function(file) {
            write.csv(sumrv$value, file, row.names = TRUE)
        }
    )
    
    output$downloadSummaryrds <- downloadHandler(
        filename = "summary.rds",
        content = function(file) {
            saveRDS(sumrv$value, file)
        }
    )
    
    output$downloadArray <- downloadHandler(
        filename = "array.txt",
        content = function(file) {
            head <- paste0("\n", arraycode(comment = TRUE))
            write.traps(detectorarray(), header = head, file)
        }
    )
    
    output$downloadSpacing <- downloadHandler(
        filename = "spacing.RData",
        content = function(file) {
            spacingOutput <- rotrv$output
            save(spacingOutput, file = file)
        }
    )
    output$downloadnrmcode <- downloadHandler(
        filename = "nrmcode.R",
        content = function(file) {
            nrmtext <- nrmcode()
            cat(nrmtext, file = file)
        }
        , contentType = "text/R"
    )
   
    outputOptions(output, "validspacing", suspendWhenHidden = FALSE)
    
    ##############################################################################
    # tidy end of session - app closes in R
    # ?apparently incompatible with bookmarking 2019-01-17
    
    # session$onSessionEnded(function() {
    #     stopApp()
    # })
    
    ##############################################################################

    setBookmarkExclude(c("simulatebtn", "simulatebtn2", "spacingbtn", "appendbtn",
                         "clearallbtn", "clearlastbtn", "selectnonebtn", "selectallbtn",
                         "suggestbtn", "suggestlinebtn", "suggestfilebtn",
                         "resetbtn", "routebtn", "randompopbtn", "randomarraybtn", 
                         "selectfieldsbtn", "selecting"))
    
    # Save extra values in state$values when we bookmark
    onBookmark(function(state) {
        shp <- sapply(c(input$regionfilename, 
                        input$exclusionfilename, 
                        input$habpolyfilename), shpfile)
        if (any(shp)) {
            showNotification("ESRI shapefile will not be bookmarked", type = "error", id = "noshapefile")
        }
        state$values$shp <- shp
        state$values$simrvoutput <- simrv$output     # does not work 
        state$values$sumrv <- sumrv$value            # works
        state$values$manualroute <- manualroute$seq
        state$values$port <- session$clientData$url_port
        ## can manually recover with e.g.
        ## readRDS('d:/density secr 3.2/secrdesignapp/shiny_bookmarks/9c88715bacc260cf/values.rds')$port
    })    
    # Read values from state$values when we restore
    onRestore(function(state) {
        simrv$output <- state$values$simrvoutput
        sumrv$value <- state$values$sumrv
        current$unit <- input$areaunit
        manualroute$seq <- state$values$manualroute
        if (any(state$values$shp)) {
            showNotification("Cannot restore ESRI shapefile(s); re-select", type = "error", id = "noshapefile2")
        }
        updateNumericInput(session, "D", paste0("D (animals / ", input$areaunit, ")"))
    })
    ##############################################################################

    ## use parameters provided in calling url
    observe({
        query <- parseQueryString(session$clientData$url_search)
        
        if (length(query)>0) {
            updateTabsetPanel(session, "arrayinput", selected = "File")
            
            if (!is.null(query[['trapfilename']])) {
                updateNumericInput(session, "trapfilename", value = query[['trapfilename']])
                if (!is.null(query[['trapargs']])) {
                    updateTextInput(session, "trapargs", query[['trapargs']])
                }
            }
            
            if (!is.null(query[['detector']])) {
                updateSelectInput(session, "detector", selected = query[['detector']])
            }
            if (!is.null(query[['noccasions']])) {
                updateNumericInput(session, "noccasions", value = as.numeric(query[['noccasions']]))
            }
            if (!is.null(query[['detectfnbtn']])) {
                updateSelectInput(session, "detectfn", selected = query[['detectfnbtn']])
            }
            if (!is.null(query[['distributionbtn']])) {
                updateRadioButtons(session, "distributionbtn", selected = query[['distributionbtn']])
            }
            if (!is.null(query[['D']])) {
                updateNumericInput(session, "D", value = as.numeric(query[['D']]))
            }
            if (!is.null(query[['lambda0']])) {
                updateNumericInput(session, "lambda0", value = as.numeric(query[['lambda0']]))
            }
            if (!is.null(query[['sigma']])) {
                updateNumericInput(session, "sigma", value = as.numeric(query[['sigma']]))
            }
            ## display notification until arrayPlot successful
            showNotification("browse to detector file or specify other array (Grid, Line, Region)", 
                             type = "message", id = "reloadtraps", duration = NULL)
            
        }
    })
}

##################################################################################
# Run the application
shinyApp(ui = ui, server = server, enableBookmarking = "server")
##################################################################################
