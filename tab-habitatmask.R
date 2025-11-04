tabhabitat <- tabPanel("Habitat mask", value = "mask_tab",
         
         fluidRow(
             column(3,
                    tabsetPanel(
                        type = "pills", id = "masktype", selected = "Build",
                        
                        tabPanel("Build",
                                 wellPanel(class = "mypanel", 
                                           fluidRow(
                                               column(6, 
                                                      numericInput("habxsigma", "buffer width (x sigma)",
                                                                   min = 0,
                                                                   max = 20,
                                                                   value = 4,
                                                                   step = 0.5,
                                                                   width = 180)),
                                               column(6,
                                                      numericInput("habnx", "mesh dimension nx",
                                                                   min = 10,
                                                                   max = 1000,
                                                                   value = 32,
                                                                   step = 1,
                                                                   width = 180)
                                               )
                                               # column(6,
                                               #        br(),
                                               #        actionButton("suggestbuffer", "Suggest width", width = 130,
                                               #                     title = "Based on either fitted model or RPSV"))
                                           ),
                                           fluidRow(
                                               column(12, 
                                                      radioButtons("maskshapebtn", label = "shape",
                                                                   choices = c("Rectangular", "Trap buffer"), 
                                                                   selected = "Trap buffer", inline = TRUE)
                                               )
                                           )
                                 ),
                                 wellPanel(class = "mypanel", 
                                           div(style="height: 80px;",
                                               fileInput("habpolyfilename", "mask polygon file(s)",
                                                         accept = c('.shp','.dbf','.sbn','.sbx',
                                                                    '.shx',".prj", ".txt", ".rdata", ".rda", ".rds"), 
                                                         multiple = TRUE)),
                                           uiOutput("habitatfile"),
                                           fluidRow(
                                               column(10, 
                                                      checkboxInput("polygonbox", "clip to polygon(s)", value = TRUE),
                                                      radioButtons("includeexcludebtn", label = "",
                                                                   choices = c("Include", "Exclude"), 
                                                                   selected = "Include", inline = TRUE)
                                               )
                                           )
                                 )
                        ),
                        tabPanel("File", 
                                 wellPanel(class = "mypanel", 
                                           div(style = "height: 80px;",
                                               fileInput("maskfilename", "Mask file",
                                                         accept = c('.txt'), 
                                                         multiple = FALSE)),
                                           helpText(HTML(paste0("Requires text file with ",
                                                                "x-y coordinates in first two columns,",
                                                                " as for secr::read.mask"))),
                                           textInput("maskargs", "Optional arguments for read.mask()",
                                                     value = "header = TRUE", placeholder = "e.g., sep = ','"),
                                           verbatimTextOutput("masktext"),
                                           tags$head(tags$style("#masktext{overflow-y:scroll; max-height: 127px; 
                                                                                     background: ghostwhite;}")),
                                           br(),
                                           fluidRow(
                                               column(6,numericInput("maskcov", "Focal covariate",
                                                                     min = 0,
                                                                     max = 0,
                                                                     value = 0,
                                                                     step = 1,
                                                                     width = 180)),
                                               column(6, br(), checkboxInput("scaleD", "Rescale density",
                                                                             value = FALSE))
                                           )
                                 )
                        ) 
                    )
             ),
             column(5, plotOutput("maskPlot"),
                    conditionalPanel ("output.maskUploaded", fluidRow(
                        column(3, offset = 1, checkboxInput("dotsbox", "dots", value = FALSE, width = 180)),
                        column(3, offset = 1, checkboxInput("xpdbox", "xpd", value = FALSE, width = 180)),
                        column(4, checkboxInput("maskedge2", "show mask edge", value = FALSE))
                    ))
             ),
             column(3, 
                    h2("Summary"),
                    fluidRow(
                        column(12, 
                               verbatimTextOutput("maskPrint"))
                    )
             )
         )
)