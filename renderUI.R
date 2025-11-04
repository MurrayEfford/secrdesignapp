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

output$Edetectors <- renderUI({
    if (!is.na(edetrv$ntraps)) {
        helpText(HTML("E(detectors) ", paste0(round(edetrv$ntraps,1))))
    }
})

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

output$model2Dhelp <- renderUI({
    helptext <- ""
    if (input$model2D == 'cluster')
        helptext <- "mu (mean no. per cluster)  &  hsigma (cluster spread, SD)"
    helpText(HTML(helptext))
})
##############################################################################

output$kprint <- renderUI({
    helptext <- ""
    
    k <- density()^0.5 * input$sigma / 100
    kstr <- if (input$detectfn=="HHN") 
        paste0("Overlap coefficient &nbsp k &nbsp =  &nbsp 0.01σ√D &nbsp = &nbsp ", round(k,3))
    else ""
    
    if (k<0.2 || k>1.5) {
        showNotification(paste0("Unlikely combination of D and sigma  (k = ", round(k,2), ")"),
                         type = "warning", id = "unlikelyk", duration = NULL)
    }
    else {
        removeNotification("unlikelyk")
    }
    
    helpText(HTML(kstr))
    
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
    if (input$arrayinput=='Region' & input$layouttype=="Random") {
        if (input$randomtype == "SRS")
            helptext <- "Simple random sample"
        else if (input$randomtype == "GRTS")
            helptext <- "Generalised random tessellation stratified sample of Stevens & Olsen (2004)"
    }
    helpText(HTML(helptext))
})
##############################################################################

# output$shapefile <- renderUI({
#     helptext <- ""
#     if (!is.null(regionrv$data)) {
#         pos <- grep(".shp", tolower(input$regionfilename[,1]))
#         if (length(pos)>0)
#             helptext <- paste0(input$regionfilename[pos,1])
#         pos <- grep(".rda", tolower(input$regionfilename[,1]))  # .rda, .rdata
#         if (length(pos)>0) {
#             objlist <- load(input$regionfilename[1,4])
#             helptext <- paste0(objlist[1])
#         }
#         pos <- grep(".rds", tolower(input$regionfilename[,1])) 
#         if (length(pos)>0) {
#             helptext <- paste0(input$regionfilename[pos,1])
#         }
#     }
#     helpText(HTML(helptext))
# })
##############################################################################

output$exclusionfile <- renderUI({
    helptext <- ""
    if (!is.null(exclusionrv$data)) {
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
    if (!is.null(habpolyrv$data)) {
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
