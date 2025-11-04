taboptions <- tabPanel("Options", value = "options_tab",
         
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
                                                        value = 2000,
                                                        step = 100))
                              ),
                              checkboxInput("lockxy", "Couple row and column dimensions", TRUE)
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
                                                       width = 180))
                              ),
                              fluidRow(
                                  column(6,
                                         checkboxInput("trafficlightbox", 
                                                       "Traffic lights", 
                                                       value = TRUE, 
                                                       width = 180))
                              )
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
                    ),
                    h2("Traffic light thresholds"),
                    wellPanel(class = "mypanel", 
                              fluidRow(
                                  column(6, numericInput("minEm", "Minimum E(m)",
                                                         min = 0,
                                                         max = 100,
                                                         value = 5,
                                                         step = 1,
                                                         width = 180))
                              ),
                              fluidRow(
                                  column(6, numericInput("maxgreen", "Max RSE% green",
                                                         min = 0,
                                                         max = 100,
                                                         value = 15,
                                                         step = 1,
                                                         width = 180)),
                                  column (6, numericInput("maxamber", "Max RSE% amber",
                                                          min = 0,
                                                          max = 100,
                                                          value = 20,
                                                          step = 1,
                                                          width = 180))
                              )
                    )
             )
         )
)