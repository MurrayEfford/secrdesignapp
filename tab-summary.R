tabsummary <- tabPanel("Summary", value = "summary_tab",
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
                                                                               "rotRSE", "CF", "route", "cost", "simfn",  "model2D", "details", 
                                                                               "nrepl", "simtime", "simRSE", "simRSEse", "simRB", "simRBse", "empiricalRSE"),
                                                                   selected = c("detperHR", "k", "En", "Er", "Em",
                                                                                "rotRSE", "CF", "route", "cost", "simfn",  "model2D", "details", 
                                                                                "nrepl", "simtime", "simRSE", "simRSEse", "simRB", "simRBse", "empiricalRSE")
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
)