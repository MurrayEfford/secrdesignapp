tabspacing <- tabPanel("Spacing", value = "spacing_tab",
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
                               helpText(HTML('See Simulation for options'))
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
                                            # Show a plot of the approximate RSE
                                            plotOutput("RSEPlot", width = "520px", height = "400px", 
                                                       hover="spacingTrafficClick")
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
         
)