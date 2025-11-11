## 2019-02-17 suppressed scaling factor for detectors from file
## 2019-02-17 suppress 'lownr' notification - rely on traffic lights
## 2020-01-07 version 1.4 drops openCR; optional lacework
## 2022-01-31 version 1.5 update documentation
## 2022-02-06 version 1.5 check compatible secr 4.5.2
## 2022-10-26 version 1.6
## 2025-11-03 version 2.0

# requires package sf to read shapefiles
# requires package sp for plot method for SpatialPolygons
# requires package parallel for max cores in simulate options (distributed with base R)
# requires package tools for file path when reading shapefiles (distributed with base R)
# requires package spsurvey >= 5.3.0 for GRTS 

library(secrdesign)
library(sf)
library(shinyjs)

source('globalvars.R',      local = TRUE)
source('tab-design.R',      local = TRUE)
source('tab-habitatmask.R', local = TRUE)
source('tab-costing.R',     local = TRUE)
source('tab-simulation.R',  local = TRUE)
source('tab-spacing.R',     local = TRUE)
source('tab-summary.R',     local = TRUE)
source('tab-options.R',     local = TRUE)
source('tab-help.R',        local = TRUE)
source('tab-about.R',       local = TRUE)

# Define UI 
ui <- function(request) {
    fluidPage(
        title = "secrdesignapp 2.0",
        includeCSS("www/secrdesignstyle.css"),
        useShinyjs(),
        withMathJax(),
        br(),
        navlistPanel("secrdesignapp 2.0",
                     id = "navlist", widths = c(2,10), well = TRUE, 
                     tabdesign, 
                     tabhabitat,
                     tabcosting,
                     tabsimulation,
                     tabspacing,
                     tabsummary,
                     taboptions,
                     tabhelp,
                     tababout
        )   
    )     
}
############################################################################################
# Define server logic

server <- function(input, output, session) {
    
    # This command forces a browser resize event 500ms after the app loads
    shinyjs::delay(
        1000, 
        shinyjs::runjs("$('#target_output_panel').trigger('resize');")
    )
    
    # source here as these use input or assign to output
    source('miscfn.R',          local = TRUE)
    source('codestringfn.R',    local = TRUE)
    source('renderUI.R',        local = TRUE)
    source('renderPlot.R',      local = TRUE)
    source('renderPrint.R',     local = TRUE)
    source('renderText.R',      local = TRUE)
    source('renderTable.R',     local = TRUE)
    source('reactiveValues.R',  local = TRUE)
    source('reactive.R',        local = TRUE)
    source('observeEvent.R',    local = TRUE)
    source('observe.R',         local = TRUE)
    source('downloadHandler.R', local = TRUE)
    # source('bookmark.R',        local = TRUE)   # disabled 2025-11-10

    output$selectingfields <- renderText('false')

    outputOptions(output, "selectingfields", suspendWhenHidden = FALSE)
    outputOptions(output, "validspacing", suspendWhenHidden = FALSE)
    outputOptions(output, "trapsUploaded", suspendWhenHidden = FALSE)
    outputOptions(output, "maskUploaded", suspendWhenHidden = FALSE)
    
    showNotification(paste("secrdesign", desc$Version, desc$Date),
                     closeButton = FALSE, type = "message", duration = seconds)
    
    ##############################################################################
    # tidy end of session - app closes in R
    # apparently incompatible with bookmarking 2019-01-17
    
    session$onSessionEnded(function() {
        stopApp()
    })
    
    ##############################################################################
    
}

##################################################################################
# Run the application
shinyApp(ui = ui, server = server, enableBookmarking = "disable")
##################################################################################
