
# spacingcodePrint
# nrmlegend   (Spacing nrm plot)
# simcodePrint
# costPrint
# spacingPrint 
# spacingPrint2
# simPrint 
# GAprint

##############################################################################
output$traptext <- renderText({
    fileText <- NULL
    if (!is.null(input$trapfilename)) {
        filePath <- input$trapfilename$datapath
        if (file.exists(filePath)) {
            fileText <- paste(readLines(filePath), collapse = "\n")
        }
    }
    fileText
})
##############################################################################

output$masktext <- renderText({
    fileText <- NULL
    if (!is.null(input$maskfilename)) {
        filePath <- input$maskfilename$datapath
        if (file.exists(filePath)) {
            fileText <- paste(readLines(filePath), collapse = "\n")
        }
    }
    fileText
})
##############################################################################

output$spacingcodePrint <- renderText ({
    if (is.null(isolate(detectorarray()))) "" # abort if no valid array
    else {
        
        ## inp <- oS2()
        invalidateOutputs()
        
        if (input$spacingsimbox) {
            simulationargs <- paste0(",\n    fit.function = '", input$packagebtn, "',\n",
                                     "    simulationR = seq(", input$fromR, ", ", input$toR, ", ", input$simbyR, "),\n",
                                     "    nrepl = ", input$nrepl, ", ",
                                     "ncores = ", input$ncores, ",\n",
                                     "    method = '", input$method, "', ",
                                     "seed = ", if (input$seed==0) "NULL" else input$seed
            )
        }
        else
            simulationargs <- ""
        
        paste0(
            "library(secrdesign)\n\n",
            
            arraycode(), "\n",
            
            "oS <- optimalSpacing(D = ", density(), ", traps = array,\n",
            "    detectpar = list(lambda0 = ", input$lambda0, ", sigma = ", input$sigma, "),\n",
            "    detectfn = '", input$detectfn, "', noccasions = ", input$noccasions, ", nrepeats = ", nrepeats(), ",\n",
            "    R = seq(", input$fromR, ", ", input$toR, ", ", input$byR, "), CF = ", round(input$CFslider,3), ", ",
            "distribution = '", tolower(input$distributionbtn), "'",  
            simulationargs,
            ")\n\n",
            
            "plot(oS)\n"
            
        )
    }
})
##############################################################################

output$nrmlegend <- renderText ({
    paste0("Expected number of individuals n\n",
           "Expected number of recaptures r\n",
           "Expected number of movement recaptures m\n")
})

##############################################################################

output$simcodePrint <- renderText ({
    if (is.null(isolate(detectorarray()))) {
        ""
    }
    else {
        D  <- density() * nrepeats()
        Ndist <- if (input$distributionbtn == 'Poisson') 'poisson' else 'fixed'
        model2D <- input$model2D
        detail <- if (model2D == 'cluster') paste0(",\n        details = list(", input$details, ")") else ""
        distncode <- if (input$packagebtn == "secr.fit")
            paste0("details = list(distribution = '", tolower(input$distributionbtn), "')")
        else
            paste0("distribution = '", tolower(input$distributionbtn), "'")
        
        RBcode <- if (input$method == "none") "" else
            paste0( 
                "  RB = output['RB','mean'] * 100,\n",
                "  RBse = output['RB','se'] * 100,\n"
            )
        fit <- input$packagebtn %in% c("secr.fit")
        countcode <- if (fit) {
            if (compareVersion(as.character(secrdesignversion), '2.6.0') < 0) "" else "summary(count(sims))$OUTPUT[[1]]\n" 
        } else
            paste0(
                "sumc <- function(x) {\n",
                "    c(n = sum(!is.na(x)),\n",
                "     mean = mean(x, na.rm = TRUE),\n",
                "     se =  sd(x, na.rm = TRUE)/ sum(!is.na(x))^0.5)}\n",
                "t(apply(sims$output[[1]], 2, sumc))\n")
        
        fitcode <- if (fit)
            paste0("output <- summary(predict(sims), fields = c('n', 'mean','se','sd'))$OUTPUT[[1]]\n",
                   "c(sims$proctime,\n",
                   ## RBcode,  suppress until nrepeat bug fixed 2019-02-15
                   "  RSE = output['RSE','mean'] * 100,\n",
                   "  RSEse = output['RSE','se'] * 100,\n",
                   "  empiricalRSE = output['RB','sd'] * 100\n",")")
        else ""
        
        paste0(
            "library(secrdesign)\n\n",
            
            "# construct detector array and mask\n",
            arraycode(), 
            maskcode("array"), "\n",
            
            "# simulate\n",
            "scen <- make.scenarios(",
            "noccasions = ", input$noccasions, ", ",
            "nrepeats = ", nrepeats(), ",\n    ",
            "D = ", density(), ", ",
            "sigma = ", input$sigma, ", ",
            "lambda0 = ", input$lambda0, ", ",
            "detectfn = '", input$detectfn, "')\n",
            
            "sims <- run.scenarios (",
            "nrepl = ", input$nrepl, ", ",
            "scenarios = scen, \n",
            "    trapset = array, maskset = mask, \n",
            "    pop.args = list(Ndist = '", Ndist, "', model2D = '", model2D, "'", detail, "),\n",
            "    fit = ", fit, 
            ", extractfn = summary, ",
            if (fit) paste0("fit.function = '", input$packagebtn, "', \n",
                            "    fit.args = list(detectfn = '", input$detectfn, "', ",
                            "method = '", input$method, "',\n", 
                            "        ", distncode, "),\n")
            else paste0("terse = TRUE, moves = true, \n"),
            
            "    ncores = ", input$ncores, ", ",
            "byscenario = FALSE, seed = ", if (input$seed==0) "NULL" else input$seed, 
            ")\n\n",
            
            "# return selected results\n",
            countcode,
            fitcode
        )                
    }
})
##############################################################################

output$ntrapPrint <- renderText({
    gr <- detectorarray()
    glength <- attr(gr, "arrayspan")
    if (!is.null(gr)) {
        if (input$arrayinput=='Region' && input$clustertype %in% c("Grid", "Line")
            && input$layouttype %in% c("Random","Systematic") ) {
            if (nrow(gr)==0)
                ncluster <- 0
            else
                ncluster <- length(unique(clusterID(gr)))
            clustertext <- paste0( " in ", ncluster, " clusters")
            cr <- "\n"
        }
        else {
            clustertext <- cr <- ""
        }
        HR95 <- 2 * circular.r(detectfn = input$detectfn)
        ratio <- glength/(input$sigma * HR95)
        if (ratio > 100) 
            ratio <- round(ratio)
        else 
            ratio <- round(ratio,2)
        paste0(nrow(gr), " ", input$detector, " detectors", clustertext, 
               "; ", cr, "diameter ", lengthstr(glength), " (", ratio, " HR95) ")
    }
    else ""
})
##############################################################################

output$GAprint <- renderText({
    optrv$message
})

output$GAmsgprint <- renderText({
    if (optrv$message == "") ""
    else if (!optrv$OK) "GA min(n,r) did not achieve n = r: increase ngen or popsize?"
})


output$costPrint <- renderText({
    if (!is.null(isolate(detectorarray()))) {
        costs <- nrm()
        if (is.null(costs)) {
            showNotification("costing failed; check parameters",
                             type = "error", id = "nocost", duration = errorduration)
            return("")
        }
        else {
            nocc1 <- input$noccasions + as.numeric(input$setupoccasion)
            paste0(
                "  Travel      ", input$currency, " ",
                round(costs$travel,2), "  (", round(arraypathlength()/1000  * 
                                                        nocc1 * nrepeats(),3), ' km)\n',
                "  Arrays      ", input$currency, " ",
                round(costs$arrays,2), "  (", nrepeats(), ") \n",
                "  Detectors   ", input$currency, " ",
                round(costs$detectors,2), "  (", nrow(isolate(detectorarray())) * nrepeats(), ") \n",
                "  Visits      ", input$currency, " ",
                round(costs$visits,2), "  (", nrow(isolate(detectorarray())) * nocc1 * nrepeats() , ") \n",
                "  Detections  ", input$currency, " ",
                round(costs$detections,2), "  (", round(costs$En+costs$Er,1), ')\n\n',
                "  Total       ", input$currency, " ",
                round(costs$totalcost, 2)
            )
        }
    }
    else ""
})
##############################################################################

output$spacingPrint <- renderText({
    if (rotrv$current) {
        temp <- rotrv$output
        paste0("Optimal spacing (relative to sigma) = ",
               round(temp$rotRSE$optimum.R,2), '\n',
               "Optimal spacing (absolute) = ",
               round(temp$rotRSE$optimum.spacing, 1), " m", '\n',
               "Minimum approximate RSE = ",
               round(temp$rotRSE$minimum.RSE*input$CFslider*100, 1), " %  (correction factor ", 
               round(input$CFslider,3), ")")
    }
    else NULL
})
##############################################################################

output$spacingPrint2 <- renderText(
    if (rotrv$current) {
        if (is.null(rotrv$output$simRSE)) {
            NULL
        }
        else {
            temp <- rotrv$output$simRSE$summary
            temp <- round(temp,4)
            paste(capture.output(temp), collapse = "\n")
        }
    }
    else NULL
)
##############################################################################

output$simPrint <- renderText({
    if (simrv$current) {
        sims <- simrv$output
        out <- paste0(
            
            "Number of replicates        = ",
            sims$nrepl, "\n",
            
            "Time for simulations        = ",
            round(sims$proctime,2), " seconds",  "\n",
            
            "Number of animals         n = ",
            round(sims$n,2), " (SE ", round(sims$n.se, 2), ")\n",
            
            "Number at >= 2 detectors n2 = ",
            round(sims$n2,2), " (SE ", round(sims$n2.se, 2), ")\n",
            
            "Number of detections    n+r = ",
            round(sims$ndet,2), " (SE ", round(sims$ndet.se, 2), ")\n",
            
            "Number of recaptures      r = ",
            round(sims$ndet-sims$n,2), "\n",
            
            "Number of moves           m = ",
            round(sims$nmov,2), " (SE ", round(sims$nmov.se, 2), ")\n")
        
        if (sims$fit) {
            out <- paste0(out,
                          "Simulated RSE               = ",
                          round(sims$RSE, 2), "%", " (SE ",  round(sims$RSEse, 2), "%)", "\n")
            
            if (sims$method != "none") {
                out <- paste0(out,
                              "Empirical RSE               = ",
                              round(sims$empRSE, 2), "%\n")
                out <- paste0(out,
                              "Simulated RB                = ",
                              preplus(round(sims$RB, 2)), "%", " (SE ",  round(sims$RBse, 2), "%)")
            }
        }
        out
    }
    else
        NULL
})
##############################################################################
output$nrmPrint <- renderText ({
    progress <- Progress$new(session, min = 1, max = 15)
    on.exit(progress$close())
    progress$set(message = 'Refreshing...',
                 detail = '')
    nrmval <- nrm()
    if (is.null(nrmval)) return (NULL)
    star <- if (nrepeats()>1) "*" else ""
    nrepeatstr <- if (nrepeats()>1) paste0("\n* ", nrepeats(), " arrays") else ""
    Pxyval <- Pxy()

    k <- density()^0.5 * input$sigma / 100
    # kstr <- if (input$detectfn=="HHN") paste0(
    #     "Overlap coefficient k = ",
    #     round(density()^0.5 * input$sigma / 100,3), '\n')
    # else ""

    esastr <- paste0("Effective sampling area = ",
                     areastr(nrmval$esa), star, " (mask ",
                     areastr(nrmval$maskarea * nrepeats()), star, ")\n")

    coststr <- if (is.null(nrmval$totalcost) || is.na(nrmval$totalcost) || (nrmval$totalcost<=0))
        ""
    else
        paste0( "\nTotal cost = ", input$currency, sprintf("%.2f", nrmval$totalcost), star)

    rotstr <- paste0("Approximate RSE = ",
                     round(nrmval$rotRSE * input$CFslider * 100, 1), "%", star,
                     " (correction factor ", round(input$CFslider,3), ")")

    simstr <- if (is.null(simrv$output) || !simrv$current)
        ""
    else
        paste0("\nSimulated RSE = ", round(simrv$output$RSE, 1), "%", star,
               " (SE ",  round(simrv$output$RSEse, 2), "%)")

    empstr <- if (is.null(simrv$output) || !simrv$current ||
                  input$method == "none" || input$model2D == 'poisson')
        ""
    else
        paste0("\nEmpirical RSE = ", round(simrv$output$empRSE, 1), "%", star)

    if (attr(isolate(detectorarray()), "arrayspan") < (5 * input$sigma)) {
        showNotification("Pathological design - array span < 5.sigma",
                         type = "warning", id = "lowspan", duration = NULL)
    }
    else {
        removeNotification("lowspan")
    }

    if (!any(is.na(c(nrmval$En, nrmval$Er, nrmval$Em)))) {
        if (nrmval$Em<5) {
            showNotification("Pathological design - E(m) less than 5",
                             type = "warning", id = "lownm", duration = NULL)
        }
        else {
            removeNotification("lownm")
        }
    }
    paste0(

        "Expected number of individuals detected n = ",
        round(nrmval$En,1), star,'\n',
        "Expected number at >=2 detectors       n2 = ",
        round(nrmval$En2,1), star,'\n',
        "Expected number of recaptures           r = ",
        round(nrmval$Er,1), star, '\n',
        "Expected number of movement recaptures  m = ",
        round(nrmval$Em,1), star,'\n',
        "Median detectors per 95% home range = ",
        nrmval$detperHR, '\n',
        esastr,
        rotstr,
        simstr,
        empstr,
        coststr,
        nrepeatstr
    )
})
##############################################################################
