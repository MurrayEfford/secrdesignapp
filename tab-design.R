tabdesign <- tabPanel("Design", value = "design_tab",
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
                                           wellPanel(class = "mypanel", 
                                                     
                                                     fluidRow(
                                                         column(5, 
                                                                numericInput("ny",
                                                                             "rows", # y dimension:",
                                                                             value = 8,
                                                                             min = 1,
                                                                             max = 40,
                                                                             step = 1,
                                                                             width = 80)),
                                                         column(6, 
                                                                numericInput("spy",
                                                                             "row space (m)",
                                                                             value = 20,
                                                                             min = 1,
                                                                             max = 100000,
                                                                             step = 1,
                                                                             width = 100)
                                                         )
                                                     ),
                                                     fluidRow(
                                                         column(5,
                                                                numericInput("nx",
                                                                             "columns", # x dimension:",
                                                                             value = 8,
                                                                             min = 1,
                                                                             max = 40,
                                                                             step = 1,
                                                                             width = 80)),
                                                         column(6,
                                                                numericInput("spx",
                                                                             "col space (m)",
                                                                             value = 20,
                                                                             min = 1,
                                                                             max = 100000,
                                                                             step = 1,
                                                                             width = 100))
                                                     ),
                                                     br(),
                                                     
                                                     fluidRow(
                                                         column(5, checkboxInput("hollow", "Hollow", FALSE)),
                                                         column(6, actionButton("suggestbtn", "Suggest spacing",
                                                                                title = "Find detector spacing at which E(n) = E(r)"))
                                                     ))
                                  ),
                                  tabPanel("Line",
                                           wellPanel(class = "mypanel", 
                                                     fluidRow(
                                                         column(6,numericInput("nline",
                                                                               "number of detectors",
                                                                               value = 20,
                                                                               min = 1,
                                                                               max = 100,
                                                                               step = 1,
                                                                               width = 120)),
                                                         column(6,numericInput("spline",
                                                                               "spacing (m)",
                                                                               value = 20,
                                                                               min = 1,
                                                                               max = 100000,
                                                                               step = 1,
                                                                               width = 100))
                                                     ),
                                                     actionButton("suggestlinebtn", "Suggest spacing",
                                                                  title = "Find detector spacing at which E(n) = E(r)"),
                                                     br(),br(),
                                                     helpText(HTML("Warning: results with linear arrays may be highly biased if home ranges are elongated and aligned"))
                                           )),
                                  tabPanel("File", values = "File",
                                           wellPanel(class = "mypanel", 
                                                     div(style="height: 80px;",
                                                         fileInput("trapfilename", "",   # Detector layout file
                                                                   accept = "text/plain")),
                                                     helpText(HTML(paste0("Requires text file with detector ID ",
                                                                          "and x-y coordinates in three columns,",
                                                                          " as for secr::read.traps"))),
                                                     textInput("trapargs", "Optional arguments for read.traps()",
                                                               value = "", placeholder = "e.g., skip = 1, sep = ','"),
                                                     verbatimTextOutput("traptext"),
                                                     tags$head(tags$style("#traptext{overflow-y:scroll; max-height: 107px; 
                                                                                     background: ghostwhite;}"))
                                           )),
                                  tabPanel("Region",
                                           wellPanel(class = "mypanel", 
                                                     
                                                     tabsetPanel(type = "tabs", id = "regiontype", selected = "Rectangle",
                                                                 tabPanel("Rectangle",
                                                                          br(),
                                                                          fluidRow(
                                                                              column(6, numericInput("length",
                                                                                                     "length (m)",
                                                                                                     value = 500,
                                                                                                     min = 10,
                                                                                                     max = 200000,
                                                                                                     step = 10,
                                                                                                     width = 100)),
                                                                              column(6,numericInput("width",
                                                                                                    "width (m)",
                                                                                                    value = 400,
                                                                                                    min = 10,
                                                                                                    max = 200000,
                                                                                                    step = 10,
                                                                                                    width = 100)
                                                                              )
                                                                          )
                                                                 ),
                                                                 tabPanel("From file(s)",
                                                                          br(),
                                                                          div(style="height: 80px;",
                                                                              fileInput("regionfilename", "Boundary file(s)",
                                                                                        accept = c('.shp','.dbf','.sbn','.sbx',
                                                                                                   '.shx',".prj", ".txt", ".rdata", 
                                                                                                   ".rda", ".rds"), multiple = TRUE)),
                                                                          # helpText(HTML("(at least xxx.shp, xxx.dbf, and xxx.shx)")),
                                                                          # uiOutput("shapefile")
                                                                 ),
                                                     )
                                                     
                                           ),
                                           wellPanel(class = "mypanel", 
                                                     fluidRow(
                                                         column(6, 
                                                                numericInput("numpgrid",
                                                                             "number of detectors",
                                                                             value = 20,
                                                                             min = 0,
                                                                             max = 20000,
                                                                             step = 5,
                                                                             width = 120)),
                                                         column(6,
                                                                numericInput(
                                                                    "seedpgrid", 
                                                                    "seed",
                                                                    value = 0,
                                                                    min = 0, 
                                                                    max = 1000000000,
                                                                    step = 1,
                                                                    width = 100))
                                                     ),
                                                     tabsetPanel(
                                                         type = "tabs", id = "layouttype", selected = "Random",
                                                         
                                                         tabPanel("Random",
                                                                  fluidRow(
                                                                      column(12, radioButtons("randomtype", label = "",
                                                                                              choices = c("SRS", "GRTS"), 
                                                                                              selected = "SRS", inline = TRUE),
                                                                             uiOutput('randomtext')
                                                                      )
                                                                  )
                                                         ),
                                                         tabPanel("Systematic",
                                                                  
                                                                  br(),
                                                                  fluidRow(
                                                                      column(6,
                                                                             numericInput("sppgrid",
                                                                                          "spacing (m)",
                                                                                          value = 30,
                                                                                          min = 0,
                                                                                          max = 200000,
                                                                                          step = 10),
                                                                             conditionalPanel("input.lacework",
                                                                                              numericInput("splace",
                                                                                                           "lace multiple",
                                                                                                           value = 5,
                                                                                                           min = 1,
                                                                                                           max = 100,
                                                                                                           step = 1),
                                                                             ),
                                                                             uiOutput('Edetectors'),
                                                                             uiOutput('clusteroverlap')
                                                                      ),
                                                                      column(6, 
                                                                             checkboxInput("randomorigin", "Random origin", FALSE),
                                                                             checkboxInput("chequerboard", "Chequerboard", FALSE),
                                                                             checkboxInput("lacework", "Lacework", FALSE)
                                                                      )
                                                                  )
                                                                  
                                                         ),
                                                         
                                                         tabPanel("GA",
                                                                  br(),
                                                                  fluidRow(
                                                                      
                                                                      column(6, 
                                                                             radioButtons("GAcriterion", label = "criterion",
                                                                                          choices = c("min(n,r)", "n2"), 
                                                                                          selected = "min(n,r)", inline = TRUE),
                                                                             radioButtons("GAfile", label = "alltraps",
                                                                                          choices = c("region", "file"), 
                                                                                          selected = "region", inline = TRUE),
                                                                             
                                                                             numericInput("GAminspace",
                                                                                          "grid spacing (m)",
                                                                                          value = 20,
                                                                                          min = 0,
                                                                                          max = 200000,
                                                                                          step = 5,
                                                                                          width = 120),
                                                                             numericInput("GAbuffer",
                                                                                          "external buffer (m)",
                                                                                          value = 0,
                                                                                          min = -200000,
                                                                                          max = 200000,
                                                                                          step = 10,
                                                                                          width = 120)
                                                                      ),
                                                                      column(6,
                                                                             br(),
                                                                             actionButton("optimizebtn", "Optimize",
                                                                                          title = "Run genetic algorithm"),
                                                                             br(),
                                                                             br(),
                                                                             numericInput("GAngen",
                                                                                          "kofnGA ngen",
                                                                                          value = 50,
                                                                                          min = 0,
                                                                                          max = 2000,
                                                                                          step = 5,
                                                                                          width = 100),
                                                                             numericInput("GApopsize",
                                                                                          "kofnGA popsize",
                                                                                          value = 50,
                                                                                          min = 0,
                                                                                          max = 2000,
                                                                                          step = 5,
                                                                                          width = 100)
                                                                      )
                                                                  ),
                                                                  fluidRow (
                                                                      verbatimTextOutput("GAprint"),
                                                                      verbatimTextOutput("GAmsgprint")
                                                                  )
                                                                  
                                                         )
                                                         
                                                     )),
                                           conditionalPanel("input.layouttype != 'GA'",
                                                            wellPanel(class = "mypanel",
                                                                      fluidRow(
                                                                          column(6, 
                                                                                 radioButtons("clustertype", label = "cluster type",
                                                                                              choices = c("Single detector", "Grid", "Line", "File"), 
                                                                                              selected = "Single detector")),
                                                                          column(6, 
                                                                                 numericInput(
                                                                                     "rotation",
                                                                                     "rotation",
                                                                                     value = 0,
                                                                                     min = 0,
                                                                                     max = 360,
                                                                                     step = 5,
                                                                                     width = 100)
                                                                                 # ,actionButton("randomarraybtn", "Randomize",
                                                                                 #     title = "Select another realisation")
                                                                          )
                                                                      )
                                                            )
                                           )
                                           
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
                                                         width = 120)
                                         # uiOutput('persqkm')
                                  ),
                                  column(6, selectInput("detectfn", "Detection function",
                                                        choices = c("HHN","HEX"),
                                                        selected = "HHN", width=120))
                              ),
                              
                              fluidRow(
                                  column(6, numericInput("lambda0",
                                                         "lambda0",
                                                         value = 0.2,
                                                         min = 0,
                                                         max = 100,
                                                         step = 0.01,
                                                         width = 120)),
                                  column(6, numericInput("sigma",
                                                         "sigma (m)",
                                                         min = 0,
                                                         max = 100000,
                                                         value = 25,
                                                         step = 1,
                                                         width = 120))),
                              uiOutput('kprint')
                    ),
                    
                    h2("General"),
                    
                    fluidRow(
                        column(6,
                               wellPanel(class = "mypanel", 
                                         numericInput("noccasions",
                                                      "Occasions",
                                                      value = 5,
                                                      min = 1,
                                                      max = 200,
                                                      step = 1,
                                                      width = 100),
                                         numericInput("nrepeats",
                                                      "Arrays",
                                                      value = 1,
                                                      min = 1,
                                                      max = 100,
                                                      step = 1,
                                                      width = 100)
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
                        # column(6, bookmarkButton(width = 130))    # removed 2025-11-10
                        column(7, helpText(HTML("&nbsp;&nbsp;F11 to toggle fullscreen")))
                    ),
                    br(),
                    fluidRow(
                        column(11, textInput("title", "", value = "", 
                                             placeholder = "scenario label for Summary")))
             ),
             
             column (4, # style='padding:0px;',
                     h2("Results"),
                     
                     fluidRow(
                         column(12,
                                fluidRow(
                                    column(11, verbatimTextOutput("nrmPrint"))
                                    # ,
                                    # column(1, style='padding:0px;',
                                    #        conditionalPanel("output.nrmPrint!= ''",
                                    #                         downloadLink("downloadnrmcode", "R")),
                                    #        br(),
                                    #        conditionalPanel("output.nrmPrint!= ''",
                                    #                         plotOutput("trafficlightPlot", height = 60, hover = "trafficClick")))
                                    # 
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
                                                                                    value = FALSE,
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
                                            ),
                                            tabPanel("Log",
                                                     br(),
                                                     fluidRow(
                                                         column(12, 
                                                                verbatimTextOutput("logPrint"))
                                                     )
                                            )
                                ))
                     )
             )
         )
)