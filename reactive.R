## reactive

## arraypathlength
## invalidateOutputs
## simarg
## detectorarray
## mask
## pop
## Pxy
## nrm

## outputs--
## validspacing
## trapsuploaded
## maskuploaded

##############################################################################

arraypathlength <- reactive({
    
    pathl <- NA
    if (input$routetype %in% c("Sequential", "TSP", "Manual")) {
        if (input$routetype=="manual")
            seq <- manualroute$seq
        else
            seq <- attr(detectorarray(), "seq")
        if (!is.null(seq)) {
            #     stop("requires seq attribute or manual route")
            if (length(seq)==1)
                pathl <- 0
            else {
                trps <- detectorarray()[seq,]
                ntrps <- nrow(trps)
                if (input$returnbox)
                    dxy <- trps - trps[c(2:ntrps, 1),]
                else
                    dxy <- trps[-1,] - trps[-ntrps,]
                d <- sqrt(apply(dxy^2,1,sum))
                pathl <- sum(d)
            }
        }
    }
    else if (input$routetype == "SumSpacing") {
        d <- as.matrix(dist(detectorarray()))
        diag(d) <- NA
        d <- apply(d,1,min, na.rm = TRUE)
        if (length(table(d)) == 1) {
            pathl <- (nrow(detectorarray())-1+ input$returnbox) * d[1]
        }
        else {
            pathl <- sum(d)   
        }
    }
    
    pathl
    
})

##############################################################################

# invalidateOutputs <- reactive({
#     simrv$current <- FALSE
#     rotrv$current <- FALSE
#     pxyrv$value <- NULL
#     updateNumericInput(session, "D", step = 10^trunc(log10(density()/50)))
# })

##############################################################################

# note change to any of simulation settings
simarg <- reactive({
    simrv$current <- FALSE
    list(
        nrepl = input$nrepl,
        ncores = input$ncores,
        seed = input$seed,
        method = input$method,
        model2D = input$model2D,
        details = input$details,
        fit = input$packagebtn %in% c('secr.fit'))}
)

##############################################################################

# Target the 'Spacing' tab link to disable/enable
spacing_tab_selector <- '#navlist a[data-value="spacing_tab"]'

detectorarray <- reactive(
    {
        simrv$current <- FALSE
        rotrv$current <- FALSE
        pxyrv$value <- NULL
        # arrrv$v  ## create dependency on randomarraybtn
        optrv$value  ## create dependency on optimizebtn
        if (!input$autorefresh) {
            optrv$message <- ""
            optrv$OK <- TRUE
            return(NULL)
        }
        trps <- NULL
        removeNotification("badarray")
        removeNotification("lowspan")
        removeNotification("lownm")
        
        if (input$arrayinput == "Grid") {
            trps <- make.grid(nx = input$nx, ny = input$ny, detector = input$detector,
                              spacex = input$spx, spacey = input$spy,
                              hollow = input$hollow, ID = "numxb")
        }
        else if (input$arrayinput == "Line") {
            trps <- make.grid(nx = input$nline, ny = 1, detector = input$detector,
                              spacing = input$spline)
        }
        else if (input$arrayinput == 'File') {
            traprv$scale <- 1.0 # input$scalefactor
            trps <- traprv$data
        }
        else if (input$arrayinput=='Region') {
            if (input$nrepeats>1) {
                updateNumericInput(session, "nrepeats", value = 1)
            }
            if (is.null(regionrv$data)) {
                trps <- NULL
            }
            else {
                if (input$randomorigin || input$layouttype == "Random") {
                    if (input$seedpgrid>0) {
                        set.seed(input$seedpgrid)
                    }
                }
                if (input$randomorigin) {
                    origin <- NULL
                }
                else {
                    origin <- sf::st_bbox(regionrv$data)[1:2] + input$sppgrid/2
                }
                if (input$clustertype == "Grid") {
                    cluster <- make.grid(nx = input$nx, ny = input$ny, detector = input$detector,
                                         spacex = input$spx, spacey = input$spy,
                                         hollow = input$hollow, ID = "numxb")
                }
                else if (input$clustertype == "Line") {
                    cluster <- make.grid(nx = input$nline, ny = 1, detector = input$detector,
                                         spacing = input$spline)
                }
                else if (input$clustertype == "File") {
                    traprv$scale <- 1.0 # input$scalefactor
                    cluster <- traprv$data
                }
                else {
                    cluster <- make.grid(1, 1, detector = input$detector)
                }
                
                #########################################################
                ## check expected number
                if (!is.na(edetrv$ntraps) && (edetrv$ntraps > input$maxdetectors*2)) {
                    showNotification("expected N detectors exceeds limit",
                                     type = "error", id = "toomany",
                                     duration = errorduration)
                    return(NULL)
                }
                #########################################################
                
                if (input$clustertype != "Single detector")
                    cluster <- rotate (cluster, input$rotation)
                
                if (input$layouttype == "Random") {   # randompoint
                    ntrps <- input$numpgrid
                    if (ntrps > 0)
                        trps <- trap.builder(n = ntrps, 
                                             method = input$randomtype,
                                             region = regionrv$data,
                                             cluster = cluster,
                                             edgemethod = input$edgemethod,
                                             exclude = exclusionrv$data,
                                             exclmethod = switch(input$edgemethod, 
                                                                 allinside = "alloutside", 
                                                                 anyinside = "anyoutside", 
                                                                 "clip"))
                }
                else if (input$layouttype == "GA") {   # genetic algorithm
                    trps <- optrv$value$optimaltraps
                }
                else {
                    if (input$lacework) {
                        if (input$clustertype == "Single detector") {
                            
                            if (input$randomorigin) {
                                origin <- NULL
                            }
                            else {
                                origin <- sf::st_bbox(regionrv$data)[1:2] + input$sppgrid/2
                            }
                            trps <- make.lacework(regionrv$data, detector = input$detector,
                                                  spacing = c(input$sppgrid*input$splace, input$sppgrid),
                                                  origin = origin, rotate = input$rotation)
                        }
                        else {
                            showNotification("Lacework uses single detectors, not clusters")
                        }
                        
                    }
                    else {
                        args <- list(spacing = input$sppgrid, 
                                     cluster = cluster, 
                                     region = regionrv$data,
                                     origin = origin,
                                     edgemethod = input$edgemethod,
                                     exclude = exclusionrv$data,
                                     exclmethod = switch(input$edgemethod, 
                                                         allinside = "alloutside", 
                                                         anyinside = "anyoutside", 
                                                         "clip"))
                        
                        if (input$chequerboard) {
                            args$order <- "y"
                            args$chequerboard <- "white"
                        }
                        
                        trps <- do.call(make.systematic, args)
                    }
                }
            }
            
        }
        else stop ("unrecognised array input")
        
        if (!is.null(trps)) {
            attr(trps, "arrayspan") <- suppressWarnings(pmax(0, max(dist(trps))))
        }
        if (!is.null(trps) && (nrow(trps) > input$maxdetectors)) {
            showNotification(paste0("more than ", input$maxdetectors, " detectors; try again"),
                             type = "warning", id = "bigarray", duration = seconds)
            trps <- NULL
        }
        
        if (!is.null(trps) && (nrow(trps) == 0)) {
            showNotification(paste0("no detectors; try again"),
                             type = "warning", id = "zeroarray", duration = seconds)
            trps <- NULL
        }
        
        if (is.null(trps) || input$arrayinput == "Region") {
            shinyjs::disable(selector = '#navlist a[data-value="spacing_tab"]')
        }
        else {
            shinyjs::enable(selector = '#navlist a[data-value="spacing_tab"]')
        }
        
        if (is.null(trps)) {
            # hideTab(inputId = "navlist", target = "Costing")
            # hideTab(inputId = "navlist", target = "Simulation")
            # better...
            shinyjs::disable(selector = '#navlist a[data-value="costing_tab"]')
            shinyjs::disable(selector = '#navlist a[data-value="simulation_tab"]')
            shinyjs::disable("simulatebtn2")
            shinyjs::disable("appendbtn")
            return (NULL)
        }
        else {
            # showTab(inputId = "navlist", target = "Costing")
            # showTab(inputId = "navlist", target = "Simulation")
            # better...
            shinyjs::enable(selector = '#navlist a[data-value="costing_tab"]')
            shinyjs::enable(selector = '#navlist a[data-value="simulation_tab"]')
            shinyjs::enable("simulatebtn2")
            shinyjs::enable("appendbtn")
            return(addsequence (trps, input$routetype))  # returns 'trps'
        }
    }
)
##############################################################################

mask <- reactive( {
    rotrv$current <- FALSE
    pxyrv$value <- NULL
    if (input$masktype=="Build") {
        if (!is.null(detectorarray())) {
            if (!maskOK()) {
                showNotification("no detectors in polygon(s); clipping switched off",
                                 type = "warning", id = "notrapsinpoly",
                                 duration = seconds)
                updateCheckboxInput(session, "polygonbox", value = FALSE)
            }
            make.mask (detectorarray(),
                       buffer = input$habxsigma * input$sigma,
                       nx = input$habnx,
                       type = if (input$maskshapebtn=='Rectangular') 'traprect' else 'trapbuffer',
                       poly = if (input$polygonbox) habpolyrv$data else NULL,
                       poly.habitat = input$includeexcludebtn == "Include",
                       keep.poly = FALSE)
        }
        else {
            NULL
        }
    }
    else {
        if (!maskOK()) {
            showNotification("no detectors in mask",
                             type = "warning", id = "notrapsinmask",
                             duration = seconds)
        }
        maskrv$data
    }
}
)
##############################################################################

pop <- reactive(
    {
        poprv$v
        core <- detectorarray()
        if (is.null(core) || (input$D == 0)) {
            return (NULL)
        }
        if (density() * maskarea(mask()) > 10000) {
            showNotification("population exceeds 10000; try again",
                             type = "error", id = "bigpop", duration = errorduration)
            return(NULL)
        }
        else removeNotification("bigpop")
        Ndist <- if (input$distributionbtn == 'Poisson') 'poisson' else 'fixed'
        model2D <- input$model2D
        if (model2D %in% c("cluster", "even")) {
            updateCheckboxInput(session, "onlymaskbox", value = FALSE)
        }
        if (model2D %in% c("cluster")) {
            detail <- eval(parse(text = paste0("list(", input$details, ")")))
        }
        else 
            detail <- NULL
        if (input$onlymaskbox) {
            if (nrow(mask())==0) {
                showNotification("incompatible mask",
                                 type = "error", id = "badmask", duration = errorduration)
                pop <- NULL
            }
            else {
                pop <- sim.popn (D = density(), core=mask(), model2D="IHP", Ndist = Ndist)
            }
        }
        else { # rectangular area
            pop <- sim.popn (D = density(), core=core, Ndist = Ndist,
                             buffer = input$habxsigma * input$sigma,
                             model2D = model2D, details = detail)
        }
        pop
    }
)
##############################################################################

Pxy <- reactive({
    # DOES NOT USE habpolyrv$data
    req(detectorarray())
    req(input$pxyborder, input$pxynx, input$detectfn, input$lambda0,
        input$sigma, input$noccasions)
    trps <- detectorarray()
    msk <- make.mask(trps, buffer = border(input$pxyborder), nx = input$pxynx)
    Pxy <- pdot(msk, trps, detectfn = input$detectfn,
                detectpar=list(lambda0=input$lambda0, sigma = input$sigma),
                noccasions = input$noccasions)
    sumPxy <- sum(Pxy)
    EPxy <- sum(Pxy^2) / sumPxy
    EPxy2 <- sum(Pxy^3) /sumPxy
    varPxy <- EPxy2 - EPxy^2
    sinuosity <- if (nrow(trps)<=1) NA else attr(trps, "arrayspan") / (spacing(trps) * (nrow(trps)-1))
    list(CVPxy = sqrt(varPxy)/EPxy, sinuosity = sinuosity, esa = sumPxy * attr(msk, "area"))
})
##############################################################################

nrm <- reactive({
    trps <- detectorarray()
    if (is.null(trps)) return (NULL)
    msk <- mask()
    if (nrow(msk)>0) {
        pathl <- arraypathlength()
        scen <- make.scenarios(trapsindex = 1, noccasions = input$noccasions, 
                               nrepeats = nrepeats(), D = density(), sigma = input$sigma, 
                               lambda0 = input$lambda0, detectfn = input$detectfn)
        scensum <- scenarioSummary(
            scen,
            trapset = trps,
            mask = msk,
            CF = 1,  
            routelength = pathl / 1000,
            costing = TRUE,
            setupoccasion = input$setupoccasion,
            unitcost = list(perkm = input$perkm,
                            perarray = input$perarray,
                            perdetector = input$perdetector,
                            pervisit = input$pervisit,
                            perdetection = input$perdetection),
            ncores = 1)
        scensum$maskarea <- maskarea(msk)
        if (input$distributionbtn == "Binomial") {
            scensum$rotRSE <- scensum$rotRSEB     
        }
        RSE <- 100*scensum$rotRSE*input$CFslider
        if (is.na(RSE))
            maxRSE <- 100
        else {
            if (RSE<10) maxRSE <- 20
            else if (RSE<20) maxRSE <- 30
            else if (RSE<30) maxRSE <- 40
            else if (RSE<40) maxRSE <- 50
            else maxRSE <- 100
        }
        if (!is.finite(RSE)) RSE <- maxRSE
        log_and_run(
            updateSliderInput(session, "RSEslider",
                          min = 1.0,
                          max = maxRSE,
                          value = RSE,
                          step = 0.1),
            "")
        
        # nr
        scensum
    }
    else {
        showNotification("invalid mask; check sigma parameter", type = "warning", id = "invalidmask",
                         duration = seconds)
        
        NULL
    }
})
##############################################################################

output$validspacing <- reactive({   ## for conditionalPanel
    rotrv$current
})   

output$trapsUploaded <- reactive({
    return(!is.null(detectorarray()))
})

output$maskUploaded <- reactive({
    return(!is.null(mask()))
})

##############################################################################

