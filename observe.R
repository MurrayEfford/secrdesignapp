##############################################################################

## observe

##############################################################################

## read trap file
observe({
    req(input$trapfilename)
    req(!traprv$clear)
    filename <- input$trapfilename[1,"datapath"]
    if (is.null(filename))
        stop("provide valid filename")
    
    trapargs <- input$trapargs
    if (trapargs != "")
        trapargs <- paste0(", ", trapargs)
    readtrapcall <-  paste0("read.traps (filename, detector = input$detector", trapargs, ")")
    OK <- try(eval(parse(text = readtrapcall)))
    if (!inherits(OK, "traps")) {
        showNotification("invalid trap file or arguments; try again",
                         type = "error", id = "badtrap", duration = seconds)
    }
    else {
        traprv$data <- OK
        if (traprv$scale != 1.0) {
            meanxy <- apply(traprv$data,2,mean)
            traprv$data[,1] <- (traprv$data[,1]- meanxy[1]) * traprv$scale + meanxy[1]
            traprv$data[,2] <- (traprv$data [,2]- meanxy[2]) * traprv$scale + meanxy[2]
        }
        
    }    
})

## read region file
observe({
    req(input$regionfilename)
    req(!regionrv$clear)
    if (input$arrayinput == 'Region') {
        if (input$regiontype == 'Rectangle') {
            box <- data.frame(x = c(0,0,1,1,0) * input$length, y = c(0,1,1,0,0) * input$width)
            regionrv$data <- secr::boundarytoSF(box) 
        }
        else {
            regionrv$data <- readpolygon(input$regionfilename) 
        }
        regionrv$area <- polyarea(regionrv$data, ha = FALSE)
    }
    else {
        regionrv$data <- NULL
        regionrv$area <- NULL
    }
})
##############################################################################

# ## make region box
observe({
    req(input$regiontype)
    if (input$arrayinput == 'Region') {
        if (input$regiontype == 'Rectangle') {
            box <- data.frame(x = c(0,0,1,1,0) * input$length, y = c(0,1,1,0,0) * input$width)
            regionrv$data <- secr::boundarytoSF(box)
            regionrv$area <- input$length * input$width
            regionrv$clear <- FALSE
        }
    }
    else {
        regionrv$data <- NULL
        regionrv$area <- NULL
    }
})
##############################################################################

## read exclusion file
observe({
    req(input$exclusionfilename)
    req(!exclusionrv$clear)
    exclusionrv$data <- readpolygon(input$exclusionfilename) 
    optrv$value <- NULL
    # optrv$message 
})
##############################################################################

## read habpoly file
observe({
    req(input$habpolyfilename)
    req(!habpolyrv$clear)
    habpolyrv$data <- readpolygon(input$habpolyfilename) 
})
##############################################################################

## read mask file
observe({
    req(input$maskfilename)
    req(!maskrv$clear)
    maskrv$data <- NULL
    if (input$masktype == 'File') {
        if (!is.null(input$maskfilename)) {
            maskargs <- input$maskargs
            if (maskargs != "") maskargs <- paste0(", ", maskargs)
            
            readmaskcall <-  paste0("read.mask (input$maskfilename[1,4]", maskargs, ")")
            OK <- try(eval(parse(text = readmaskcall)))
            if (!inherits(OK, 'mask')) {
                showNotification("invalid mask file or arguments; try again",
                                 type = "error", id = "badmask", duration = seconds)
            }
            else {
                maskrv$data <- OK
            }
        }
        covar <- covariates(maskrv$data)
        if (!is.null(covar)) {
            updateNumericInput(session, "maskcov", max = ncol(covar))
        }
    }
})
##############################################################################

observe({
    if (input$lockxy) {
        updateNumericInput(session, "spx", value = input$spy)
        updateNumericInput(session, "nx", value = input$ny)
    }
})
##############################################################################

## use parameters provided in calling url
observe({
    query <- parseQueryString(session$clientData$url_search)
    
    if (length(query)>0) {
        # suppress 2019-07-05: no longer needed?
        # updateTabsetPanel(session, "arrayinput", selected = "File")
        
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
        # suppress 2019-07-05: no longer needed?
        ## display notification until arrayPlot successful
        # showNotification("browse to detector file or specify other array (Grid, Line, Region)", 
        #                  type = "message", id = "reloadtraps", duration = NULL)
        
    }
})
