## observeEvent

# partial list:

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
expecteddetectors <- function() {

    removeNotification('toomany')
    removeNotification("outsideregion")
    # assuming Region with Systematic or GA 
    if (input$arrayinput == 'Region') {
        A <- regionrv$area       # m^2
        k <- NA
        if (input$layouttype == 'Systematic') {
            if (input$lacework) {
                k <- Klace(A, input$sppgrid * input$splace, input$sppgrid)
            }
            else {
                k <- A/input$sppgrid^2 
                
                if (input$chequerboard) {
                    k <- k / 2
                }
                
                if (input$clustertype == "Grid") {
                    npercluster <- nrow(make.grid(nx = input$nx, ny = input$ny, 
                                                  detector = input$detector,spacex = input$spx, 
                                                  spacey = input$spy,hollow = input$hollow))
                }
                else if (input$clustertype == "Line") {
                    npercluster <- nrow(make.grid(nx = input$nline, ny = 1, 
                                                  detector = input$detector, spacing = input$spline))
                }
                else if (input$clustertype == "File") {
                    npercluster <- nrow(traprv$data)
                }
                else {
                    npercluster <- 1
                }
                k <- k * npercluster  # ignoring edge truncation
            }
            edetrv$ntraps <- k
            edetrv$alltrapsmask <- NULL
        }
        else if (input$layouttype == 'GA') {
            if (input$GAfile =='region') {
                edetrv$alltrapsmask <- make.mask(spacing = input$GAminspace, 
                                                 type = 'polygon', poly = regionrv$data)
            }
            else {
                if (is.null(traprv$data)) {
                    edetrv$alltrapsmask <- NULL
                    optrv$message <- "no trap file loaded - see 'File'"
                }
                else {
                    edetrv$alltrapsmask <- as.mask(traprv$data)  # from file
                    inregion <- pointsInPolygon(edetrv$alltrapsmask, regionrv$data)
                    if (!all(inregion)) {
                        showNotification(
                            paste0(sum(!inregion), " locations from file are outside region"),
                            type = "error", id = "outsideregion",
                            duration = errorduration
                        )
                    }
                }
            }
            if (!is.null(edetrv$alltrapsmask)) {
                k <- nrow(edetrv$alltrapsmask)
                if (k > input$maxdetectors*10) {
                    showNotification(
                        paste0("number of potential locations exceeds limit ", 
                               input$maxdetectors*10), type = "error", id = "toomany",
                        duration = errorduration)
                }
                optrv$message <- paste0("Number of possible detector locations = ", round(k,1))
            }
            edetrv$ntraps <- NA
        }
    }
    else {
        edetrv$ntraps <- NA
        edetrv$alltrapsmask <- NULL
        optrv$message <- ""
    }
}

observeEvent(c(input$arrayinput, input$regiontype, input$layouttype, 
               input$sppgrid, input$splace, input$lacework, input$chequerboard,
               input$GAminspace, input$GAcriterion, input$GAbuffer, input$numpgrid,
               input$clustertype, input$randomorigin, input$GAfile), {
                   
                   if (input$arrayinput == 'Region') {
                       if (input$layouttype == 'Random' ||
                           (input$layouttype == 'Systematic' && input$randomorigin) ||
                           (input$layouttype == 'GA')) {
                           shinyjs::enable('seedpgrid')
                       }
                       else {
                           shinyjs::disable('seedpgrid')
                       }
                       if (input$layouttype %in% c('Random', 'GA')) {
                           shinyjs::enable('numpgrid')
                       }
                       else {
                           shinyjs::disable('numpgrid')
                       }
                       
                       if (input$layouttype == 'GA') {
                           if (input$GAfile == 'file')
                               shinyjs::disable('GAminspace')
                           else
                               shinyjs::enable('GAminspace')
                       }
                   }
                   
                   optrv$value <- NULL    
                   expecteddetectors()
                   
               })
##############################################################################

observeEvent(input$trapfilename, {
    traprv$clear <- FALSE
}, priority = 1000)
##############################################################################

observeEvent(input$regionfilename, {
    regionrv$clear <- FALSE
}, priority = 1000)
##############################################################################

observeEvent(input$exclusionfilename, {
    exclusionrv$clear <- FALSE
}, priority = 1000)
##############################################################################

observeEvent(input$habpolyfilename, {
    habpolyrv$clear <- FALSE
}, priority = 1000)
##############################################################################

observeEvent(input$maskfilename, {
    maskrv$clear <- FALSE
}, priority = 1000)
##############################################################################


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
                         type = "error", id = "nosuggest", duration = errorduration)
    }
    else {
        optimalspacing <- n.eq.r()
        if (!is.na(optimalspacing)) {
            updateNumericInput(session, "spy", value = optimalspacing)
            updateNumericInput(session, "spx", value = optimalspacing)
        }
    }
    # Force redraw after R code finishes
    shinyjs::runjs("$('#target_output_panel').trigger('resize');")
})
##############################################################################

observeEvent(input$suggestlinebtn, {
    ## Line
    ## E[n] == E[r]
    if (!input$autorefresh) {
        showNotification("enable auto refresh",
                         type = "error", id = "nosuggestline", duration = errorduration)
    }
    else {
        optimalspacing <- n.eq.r()
        if (!is.na(optimalspacing)) {
            updateNumericInput(session, "spline", value = optimalspacing)
        }
    }
})

##############################################################################

# observeEvent(input$suggestfilebtn, {
#     ## Grid
#     ## E[n] == E[r]
#     if (!input$autorefresh) {
#         showNotification("enable auto refresh",
#                          type = "error", id = "nosuggestfile", duration = seconds)
#     }
#     else {
#         optimalfactor <- n.eq.r()
#         if (!is.na(optimalfactor)) {
#             updateNumericInput(session, "scalefactor", value = round(optimalfactor,2))
#         }
#     }
# })
##############################################################################

observeEvent(input$resetbtn, {
    
    ## DOES NOT RESET FILE INPUTS
    ## SEE E.G. https://groups.google.com/forum/#!topic/shiny-discuss/HbTa4v612FA
    
    current$unit <- "ha"
    
    shinyjs::enable("randomorigin")
    shinyjs::enable("chequerboard")
    
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
    
    ## file
    updateTextInput(session, "trapargs", value = "", 
                    placeholder = "e.g., skip = 1, sep = ','")
    # updateNumericInput(session, "scalefactor", value = 1.0)
    
    ## region
    updateTabsetPanel(session, inputId = "regiontype", selected = "Rectangle")
    updateTabsetPanel(session, inputId = "layouttype", selected = "Random")
    updateNumericInput(session, "length", value = 500 )
    updateNumericInput(session, "width", value = 400 )
    updateNumericInput(session, "sppgrid", value = 30 )
    updateNumericInput(session, "splace", value = 5 )
    updateRadioButtons(session, "clustertype", selected = "Single detector" )
    updateNumericInput(session, "rotation", value = 0)
    updateCheckboxInput(session, "lacework", value = FALSE )
    updateCheckboxInput(session, "chequerboard", value = FALSE )
    updateCheckboxInput(session, "randomorigin", value = FALSE )
    updateRadioButtons(session, "randomtype", selected = "SRS")
    updateNumericInput(session, "numpgrid", value = 20)
    updateNumericInput(session, "seedpgrid", value = 0)
    
    updateNumericInput(session, "GAminspace", value = 20)
    updateNumericInput(session, "GAbuffer", value = 0)
    updateNumericInput(session, "GAngen", value = 50)
    updateNumericInput(session, "GApopsize", value = 50)
    updateRadioButtons(session, "GAcriterion", selected = "min(n,r)")
    updateRadioButtons(session, "GAfile", selected = "region")
    
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
    updateCheckboxInput(session, "powertype", "95% CI", value = FALSE)
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
    updateNumericInput(session, "nrepl", value = defaultrepl)
    updateNumericInput(session, "ncores", value = defaultcores)
    updateNumericInput(session, "seed", value = 0)
    
    updateCheckboxGroupInput(session, "model2D", selected = 'poisson')
    updateTextInput(session, "details", value = '')
    
    updateRadioButtons(session, "method", selected = "none")
    updateRadioButtons(session, "packagebtn", selected = "secr.fit")
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
    
    updateTextInput(session, "maskargs", value = "header = TRUE", placeholder = "e.g., sep = ','")
    
    updateCheckboxInput(session, "scaleD", value = FALSE)
    updateNumericInput(session, "maskcov", value = 0)
    
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
    
    updateNumericInput(session, "maxgreen", value = 15)
    updateNumericInput(session, "maxamber", value = 20)
    
    updateRadioButtons(session, "powerplotbtn", selected = "Null hypothesis power")
    
    updateNumericInput(session, "alpha", value = 0.05)
    updateNumericInput(session, "target", value = 80)
    updateSelectInput(session, "testtype", selected = "two.sided")
    updateNumericInput(session, "minEffect", value = -99)
    updateNumericInput(session, "maxEffect", value = 150)
    
    updateCheckboxInput(session, "trafficlightbox", value = TRUE)
    updateNumericInput(session, "fromR", value = 0.2)
    updateNumericInput(session, "toR", value = 4)
    updateNumericInput(session, "byR", value = 0.2)
    updateNumericInput(session, "simbyR", value = 0.4)
    
    invalidateOutputs()
    
    traprv$data <- NULL
    traprv$clear <- TRUE
    reset('trapfilename')
    
    regionrv$data <- NULL
    regionrv$area <- NULL
    regionrv$clear <- TRUE
    reset('regionfilename')
    
    optrv$value <- NULL
    optrv$message <- ""
    optrv$OK <- TRUE
    
    exclusionrv$data <- NULL
    exclusionrv$clear <- TRUE
    reset('exclusionfilename')
    
    habpolyrv$data <- NULL
    habpolyrv$clear <- TRUE
    reset('habpolyfilename')
    
    maskrv$data <- NULL
    maskrv$clear <- TRUE
    reset('maskfilename')
    
    simrv$output <- NULL
    
    log_data$messages <- character(0)
    shinyjs::runjs("$('#target_output_panel').trigger('resize');")
    
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
                         type = "error", id = "oldsecr", duration = errorduration)
        updateCheckboxInput(session, "chequerboard", value = FALSE)
    }
})

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

observeEvent(input$spacingbtn, {
    if (input$spacingsimbox) {
        
        ## time check 2018-12-05
        methodfactor <- 1 + ((input$method != "none") * 4)
        functionfactor <- 4
        detectorfactor <- switch(input$detector, proximity = 1, multi = 0.6, count = 4)
        time <- nrow(mask()) * nrow(isolate(detectorarray())) / 1e9 * ## blocked 2019-01-14 nrepeats() * 
            nrm()$En * input$noccasions * input$nrepl * 
            length(seq(input$fromR, input$toR, input$simbyR)) *
            methodfactor * functionfactor * detectorfactor
        ## 2019-03-13, removed 2025-11-11
        ## arbitrarily boost expected time because server slow
        ## time <- time * 3
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

observeEvent(input$model2D, {
    if (input$model2D == 'cluster') {
        if (input$details == '') {
            updateTextInput(session, "details", value = 'mu = 1, hsigma = 0')
        }
    }
    else if (input$model2D %in% c('poisson','even')) {
        updateTextInput(session, "details", value = '')
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

observeEvent(c(input$simulatebtn, input$simulatebtn2), ignoreInit = TRUE, {
    ## ignoreInit blocks initial execution when simulatebtn2 goes from NULL to 0
    if (!is.null(isolate(detectorarray()))) {
        methodfactor <- 1 + ((input$method != "none") * 4)
        functionfactor <- switch(input$packagebtn, secr.fit = 4, 0.1)
        detectorfactor <- switch(input$detector, proximity = 1, single = 0.6, multi = 0.6, count = 4)
        En <- nrm()$En
        if (is.na(En)) En <- 100  ## surrogate for 'single' detectors
        time <- nrow(mask()) * nrow(isolate(detectorarray())) / 4.5e9 * ## blocked 2019-01-14 nrepeats() * 
            En * input$noccasions * input$nrepl * 
            methodfactor * functionfactor * detectorfactor
        ## 2019-03-13
        ## arbitrarily boost expected time because server slow
        time <- time * 3
        if (time > 0.2)
            showModal(OKModal(time))
        else {
            runsims()
        }
    }
})
##############################################################################

observeEvent(c(input$D, input$detectfn, input$lambda0, input$sigma, 
               input$noccasions, input$nrepeats, input$distributionbtn), {
                   simrv$current <- FALSE
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

trafficNotification <- function (colour, RSE, Em) {
    if (!is.na(colour) & !is.na(Em)) {
        if (colour>2) {
            if (Em < 5) showNotification("Em < 5", id = "traffic", duration = seconds)   
            else if (RSE>0.2) showNotification("RSE > 20%", id = "traffic", duration = seconds)   
            else showNotification("Array diameter < HR95", id = "traffic")
        }
        else if (colour>1) {
            showNotification("RSE > 15%", id = "traffic", duration = seconds)   
        }
        else {
            showNotification("RSE <= 15%", id = "traffic", duration = seconds)   
        }
    }
}
##############################################################################

observeEvent(input$trafficClick, {
    RSE <- nrm()$rotRSE * input$CFslider
    Em <- nrm()$Em
    colour <- trafficlight(attr(isolate(detectorarray()), "arrayspan"), 
                           input$sigma, 
                           Em,
                           RSE)
    trafficNotification (colour, RSE, Em)        
})
##############################################################################

observeEvent(input$spacingTrafficClick, {
    req(rotrv$output)
    R <- seq(input$fromR, input$toR, input$byR)
    Rout <- input$spacingTrafficClick$x       
    RSE <- approx(R, rotrv$output$rotRSE$values$RSE, Rout)$y
    Em <- approx(R, rotrv$output$rotRSE$values$m, Rout)$y
    colour <- trafficlight(attr(isolate(detectorarray()), "arrayspan") * Rout,
                           input$sigma, 
                           Em, 
                           RSE)
    trafficNotification (colour, RSE, Em)        
})
##############################################################################

observeEvent(input$randompopbtn, ignoreInit = TRUE, {
    # invalidates pop when button pressed
    poprv$v <- poprv$v + 1
})

##############################################################################
# observeEvent(input$randomarraybtn, ignoreInit = TRUE, {
#     # invalidates detectorarray when button pressed
#     arrrv$v <- arrrv$v + 1
# })


##############################################################################
observeEvent(input$optimizebtn, ignoreInit = TRUE, {
    
    if (!is.null(edetrv$alltrapsmask) && 
        nrow(edetrv$alltrapsmask) > 10*input$maxdetectors) {
        optrv$value <- NULL
        optrv$message <- "requested potential locations exceed maximum"
    }
    else {
        temppoly <- regionrv$data
        if (input$GAbuffer != 0) temppoly <- sf::st_buffer(temppoly, input$GAbuffer)
        msk <- make.mask(buffer = 0, nx = input$habnx, type = 'polygon', poly = temppoly)
        alltraps <- read.traps(data = edetrv$alltrapsmask, detector = input$detector)
        
        detectpar <- list(lambda0 = input$lambda0, sigma = input$sigma)
        optrv$value <- NULL  # wipe clean
        progress <- Progress$new(session, min = 1, max = 15)
        on.exit(progress$close())
        progress$set(message = 'Optimizing...', detail = '')
        
        ntrps <- input$numpgrid
        if (ntrps > 0) {
            # https://stackoverflow.com/questions/30474538/possible-to-show-console-messages-written-with-message-in-a-shiny-ui
            # is only for messages
            
            msg <- log_and_run(
                expr = capture.output( 
                optrv$value <- GAoptim (
                    mask       = msk,
                    alltraps   = alltraps,
                    ntraps     = ntrps, 
                    detectpar  = detectpar, 
                    noccasions = input$noccasions, 
                    detectfn   = input$detectfn,
                    D          = input$D,
                    criterion  = if (input$GAcriterion=='min(n,r)') 4 else 5, 
                    penalty    = NULL,
                    seed       = if (input$seedpgrid == 0) NULL else input$seedpgrid,
                    ngen       = input$GAngen,
                    popsize    = input$GApopsize, 
                    verbose    = 1)
            ),
            "GAoptim"
            )
            msg <- msg[length(msg)]  # last one
            msg <- gsub("  ", " ", msg, fixed = TRUE)
            msg <- gsub(" .", ".", msg, fixed = TRUE)
            optrv$message <- substring(msg, 1, nchar(msg)-4)  # reduce dec.places
            optrv$OK <- (input$GAcriterion=='n2' || (abs(diff(optrv$value$optimalenrm[1:2]))<=1))
            log_and_run(message(optrv$message), "") # log message
        }
        else optrv$message <- ""
    }
})

##############################################################################

observeEvent(input$routebtn, {
    manualroute$seq <- NULL
}   )

##############################################################################

observeEvent(input$lacework, ignoreInit = TRUE, {
    if (input$lacework) {
        if (compareVersion(as.character(secrversion), '4.1.1') < 0) {
            updateCheckboxInput(session, "lacework", value = FALSE)
            showNotification("Lacework requires secr > 4.1.0",
                             type = "warning", id = "nolacework", duration = NULL)
        }
        else {
            updateRadioButtons(session, "clustertype", choices = 
                                   c("Single detector"), selected = "Single detector")
            updateCheckboxInput(session, "randomorigin", "Random origin", TRUE)
            shinyjs::disable("chequerboard")
            shinyjs::disable("randomorigin")
        }
    }
    else {
        updateRadioButtons(session, "clustertype", choices = 
                               c("Single detector", "Grid", "Line", "File"), 
                           selected = "Single detector")
        shinyjs::enable("chequerboard")
        shinyjs::enable("randomorigin")
    }
})

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
    shinyjs::runjs("$('#target_output_panel').trigger('resize');")
})

##############################################################################

observeEvent(c(input$arrayinput, input$resetCFbtn), {
    if (input$arrayinput != "Region") {
        removeNotification(id = "nofile")
        removeNotification(id = "nosf")
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
    trp <- nearesttrap(xy, isolate(detectorarray()))
    manualroute$seq <- c(manualroute$seq, trp)
})

##############################################################################

observeEvent(input$pxyclick, {
    invalidateOutputs()
    trps <- isolate(detectorarray())
    
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
    if (!is.null(isolate(detectorarray())))
        addtosummary()
})

##############################################################################
