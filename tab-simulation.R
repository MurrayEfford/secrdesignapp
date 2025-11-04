tabsimulation <- tabPanel("Simulation", value = "simulation_tab",
         fluidRow(
             column(5,
                    h2("Simulation control"),
                    fluidRow(
                        column(7, 
                               wellPanel(class = "mypanel", 
                                         radioButtons("packagebtn", label = "Fit SECR model", 
                                                      choices = c("secr.fit", "no fit"), 
                                                      selected = "secr.fit", inline = TRUE),
                                         radioButtons("method", label = "Maximization method",
                                                      choices = c("Newton-Raphson", "Nelder-Mead", "none"),
                                                      selected = "none", inline = TRUE),
                                         br(),
                                         fluidRow(
                                             column(5, selectInput("model2D", "2-D distribution",
                                                                   choices = c("poisson","cluster", "even"),
                                                                   selected = "poisson", width=160)),
                                             column(7, textInput("details", "other details", value = "",
                                                                 placeholder = "sim.popn argument"))
                                         ),
                                         uiOutput('model2Dhelp')
                               )
                        ),
                        column(5,
                               wellPanel(class = "mypanel", 
                                         
                                         fluidRow(
                                             column(6, numericInput("nrepl",
                                                                    "Replicates",
                                                                    min = 1,
                                                                    max = 1000,
                                                                    value = defaultrepl,
                                                                    step = 1,
                                                                    width = 180)),
                                             column(6, numericInput("ncores", "Cores", 
                                                                    min = 1, 
                                                                    max = availablecores, 
                                                                    step = 1, 
                                                                    value = defaultcores,
                                                                    width = 180))),
                                         fluidRow(
                                             column(6, checkboxInput("simappendbox", 
                                                                     "Add to Summary", TRUE)),
                                             column(6, numericInput("seed",
                                                                    "Random seed",
                                                                    min = 0,
                                                                    max = 1e10,
                                                                    value = 0,
                                                                    step = 1,
                                                                    width = 180))
                                             
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
)