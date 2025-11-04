
## renderPlot

## arrayPlot
## routePlot
## detnPlot
## popPlot
## pxyPlot
## powerPlot
## RSEPlot
## nrmPlot
## costPlot
## maskPlot


output$arrayPlot <- renderPlot( height = 340, width = 500, {   # 340
    tmpgrid <- detectorarray()
    if (is.null(tmpgrid)) {
        removeNotification("lowspan")
        removeNotification("lownr")
        # plot region outline as preliminary
        if (input$arrayinput=='Region') {
            if (!is.null(regionrv$data)) {
                par(mar = c(1,1,1,1), xpd = TRUE)
                if (input$layouttype == 'GA') {
                    if (!is.null(edetrv$alltrapsmask) && 
                        (nrow(edetrv$alltrapsmask) < input$maxdetectors*20)) {
                        temppoly <- regionrv$data
                        if (input$GAbuffer != 0) {
                            temppoly <- sf::st_buffer(temppoly, input$GAbuffer)
                        }
                        plot(temppoly, border = 'black')
                        plot(edetrv$alltrapsmask, ppoly = FALSE, cex = 0.7, add = TRUE)
                        plot(regionrv$data, border = 'black', add = TRUE )
                    }
                }
                else {
                    plot(regionrv$data, border = 'black' )
                }
            }
        }
        else {
            edetrv$ntraps <- NA
            edetrv$alltrapsmask <- NULL
        }
        return (NULL)
    }
    removeNotification("reloadtraps")
    par(mar = c(1,1,1,1), xpd = TRUE)
    if (input$arrayinput=='Region') {
        if (input$entireregionbox) {
            plot(regionrv$data)
            if (input$layouttype == 'GA') {
                plot(edetrv$alltrapsmask, ppoly = FALSE, cex = 0.7, add = TRUE)
            }
        }
        else {
            plot (tmpgrid, gridlines = (input$gridlines != "None"), gridspace = as.numeric(input$gridlines))
        }
        if (!is.null(exclusionrv$data))
            plot(exclusionrv$data, add = TRUE, col = 'lightblue', border = 'lightblue')
        plot(regionrv$data, add = TRUE)
        plot (tmpgrid, add = TRUE,  gridlines = (input$gridlines != "None"), gridspace = as.numeric(input$gridlines))
    }
    else {
        plot (tmpgrid, border = border(1), bty='o', xaxs = 'i', yaxs = 'i',
              gridlines = (input$gridlines != "None"), gridspace = as.numeric(input$gridlines))
    }
})
##############################################################################

trafficlight <- function(arrayspan, sigma, Em, RSE) {
    if (!is.null(Em)) {
        badarray <- (arrayspan / sigma) < (circular.r(detectfn = input$detectfn)*2)
        badarray <- badarray | (Em < input$minEm)
        RSE <- rep(RSE, length.out = length(badarray)) 
        colour <- rep(1, length(badarray))
        colour[(RSE>(input$maxamber/100)) | badarray] <- 3
        colour[(colour==1) & (RSE>(input$maxgreen/100))] <- 2
        colour
    }
    else {
        NULL
    }
}

output$trafficlightPlot <- renderPlot( height = 60, width = 20, {
    if (!is.null(nrm())) {
        colour <- trafficlight(attr(detectorarray(), "arrayspan"), 
                               input$sigma, 
                               nrm()$Em, 
                               nrm()$rotRSE*input$CFslider)
        radius <- 0.45
        par(mar=c(0,0,0,0))
        rect(0,0,20,60, col= grey(0.6), border = NA)
        symbols(x = rep(0.50,3), y = 0.2 + (0:2) * 0.32, circles= rep(radius,3), # fg = grey(0.93), 
                bg = grey(0.93), inches = FALSE, add = TRUE)
        symbols(x = 0.50, y = 0.2 + (colour-1)*0.32, circles= radius, # fg = trafficcols[colour], 
                bg = trafficcols[colour], inches = FALSE, add = TRUE)
    }
})

output$routePlot <- renderPlot( height = 320, width = 300, {
    tmpgrid <- detectorarray()
    if (is.null(tmpgrid)) return (NULL)
    pathl <- arraypathlength()
    seq <- attr(tmpgrid, "seq")
    if (input$returnbox) seq <- c(seq,1)
    par(mar=c(3,0,0,2))
    plot (tmpgrid, border = border(1), gridlines = FALSE, bty='o', xaxs='i', yaxs='i')
    lines(tmpgrid[seq,], col = 'grey')
    plot(tmpgrid, add = TRUE)  ## overplot symbols
    if (!is.null(pathl))
        mtext(side=1, line=1.5, paste0("Length of route = ", round(pathl/1000,3), " km"),
              xpd = TRUE, adj = 0.5, cex = 1.1)
})
##############################################################################

output$detnPlot <- renderPlot( height = 290, width = 400, {
    ## inp <- oS2()
    invalidateOutputs()
    
    par(mar=c(4,5,2,5))
    detectfnplot (detectfn = input$detectfn,
                  pars = c(input$lambda0, input$sigma),
                  xval = 0:(3 * input$sigma),
                  ylab = "",
                  hazard = TRUE,       ## 2017-08-28
                  ylim = c(0, input$lambda0*1.2),
                  las=1, col = 'red', lwd = linewidth,
                  xaxs='i', yaxs='i')
    mtext(side = 2, line = 3.7, expression(paste("Detection hazard   ", lambda [0])))
    
    if (input$lambda0 <= 0.7) 
        p <- seq(0,1,0.05)
    else 
        p <- seq(0,1,0.1)
    
    axis(4, at = -log(1 - p), label = p, xpd = FALSE, las = 1)
    mtext(side = 4, line = 3.7, "Detection probability")
})
##############################################################################

output$popPlot <- renderPlot( height = 300, width = 380, {
    core <- detectorarray()
    if (is.null(core)) return (NULL)
    border <- input$habxsigma * input$sigma  # consistent with mask()
    tmppop <- pop()
    n <- if (is.null(tmppop)) 0 else nrow(tmppop)
    tmpsig <- input$sigma
    #par(mar=c(0,1,0,1))
    if (input$pxyfillbox) {
        par(mar=c(0,1,0,5)) # , xaxs='i', yaxs='i')
    }
    else {
        par(mar=c(0,3,0,3), xaxs='i', yaxs='i', xpd = FALSE)
    }
    
    if (input$showmaskbox) {
        plot (core, border = border, gridlines = FALSE)
        plot (mask(), add = TRUE, col = grey(0.9), dots=F)
        if (n>0) plot(tmppop, add = TRUE, pch = 16, cex = 0.7, xpd = TRUE, frame = FALSE)
        plot (core, add = TRUE)
    }
    else {
        plot (core, border = border, gridlines = FALSE)
        if (n>0) plot(tmppop, pch = 16, cex = 0.7, xpd = TRUE, add = TRUE, frame = FALSE)
        plot (core, add = TRUE)
        bbox <- sweep(apply(core, 2, range), MAR=1, STATS = c(-border,+border), FUN="+")
        bbox <- expand.grid(data.frame(bbox))[c(1,2,4,3),]
        polygon(bbox)
    }
    if (input$showHRbox & (n>0)) {
        # rad <- rep(2.45 * tmpsig, nrow(tmppop))
        rad <- secr::circular.r(p = 0.95, detectfn = input$detectfn, sigma = tmpsig)
        symbols(tmppop$x, tmppop$y, circles = rep(rad, n),
                inches = FALSE, fg = grey(0.7), add = TRUE, xpd = FALSE)
    }
})
##############################################################################

border <- function (multiple) {
    spc <- spacing(detectorarray()) 
    if (is.null(spc) || is.na(spc) || length(spc) == 0) spc <- input$sigma
    multiple * spc
}

output$pxyPlot <- renderPlot( height = 300, width = 380, {
    core <- detectorarray()
    if (is.null(core)) return (NULL)
    invalidateOutputs()
    
    if (input$pxyfillbox) {
        cols <- terrain.colors(11)
        col <- cols[1]
        lev <- c(0.01, seq(0.1, 0.9, 0.1))
        par(mar=c(0,1,0,5)) # , xaxs='i', yaxs='i')
    }
    else {
        col <- "blue"
        cols <- NULL
        lev <- seq(0.1, 0.9, 0.1)
        par(mar=c(0,3,0,3)) # , xaxs='i', yaxs='i')
    }
    
    border <- border(input$pxyborder)
    plot(core, border = border, gridlines = FALSE, hidetr = TRUE)
    
    xr <- range(core[,1]) + c(-1,1) * border
    yr <- range(core[,2]) + c(-1,1) * border
    
    if (input$pxyfillbox) {
        # clunky way to give green background
        rect(xr[1],yr[1],xr[2],yr[2], col = cols[1], border = NA)
        drawlabels <- FALSE
    }
    else
        drawlabels <- input$pxylabelbox
    
    pdotContour(core, border = border, nx = input$pxynx,
                detectfn = input$detectfn,
                detectpar = list(sigma = input$sigma, lambda0 = input$lambda0),
                noccasions = input$noccasions, drawlabels = drawlabels,
                binomN = NULL, levels = lev, 
                poly = if (input$polygonbox) habpolyrv$data else NULL, 
                poly.habitat = input$includeexcludebtn == "Include",
                plt = TRUE, add = TRUE,
                col = col, fill = cols)
    plot (core, add = TRUE)
    if (!is.null(pxyrv$value)) {
        xy <- pxyrv$xy
        points(xy[1], xy[2], pch=16, cex=0.7)
        offset <- (par()$usr[2] - par()$usr[1])/15
        text(xy[1]+offset, xy[2], round(pxyrv$value,3), cex = 0.9, xpd = TRUE)
    }
    if (input$pxyframebox) {
        rect(xr[1],yr[1],xr[2],yr[2], col = NA, border = "black") 
    }
    if (input$pxyfillbox) {
        # strip.legend("right", legend = seq(0,1,0.1), title = "p.(x)", xpd = TRUE,
        #              legendtype='breaks', inset = 0.01, col = cols[3:12])
        strip.legend("right", legend = c(0,lev[1:10],1), title = "p.(x)", xpd = TRUE,
                     legendtype='breaks', inset = 0.01, col = cols[1:11])
        
    }
})
##############################################################################

output$powerPlot <- renderPlot( height = 320, width = 360, {
    RSE <- input$RSEslider/100
    if (input$powertype) {    ## confidence interval
        par(mar=c(4,4,2,2), mgp=c(2.4,0.7,0))
        headroom <- (input$maxEffect-input$minEffect)/4
        powLU <- plotpowerCI(RSE = RSE, effectRange=c(input$minEffect, input$maxEffect),
                             estimatedRange = c(input$minEffect, input$maxEffect+headroom),
                             adjustRSE = input$adjustRSEbox, alpha = input$alpha)
        x <- input$xpos 
        y1 <- approx(x = as.numeric(dimnames(powLU$limits)[[1]]), y = powLU$limits[,1,1], xout = x)$y*100-100
        y2 <- approx(x = as.numeric(dimnames(powLU$limits)[[1]]), y = powLU$limits[,2,1], xout = x)$y*100-100
        segments(x, y1, x, y2, lwd= linewidth, col = "blue")
        text(rep(x,2)+5, c(y1+1, min(y2+1, par()$usr[4]*1.06)), round(c(y1, y2)), adj = 0, cex = 0.9, col = "blue", xpd = TRUE)
    }
    else {
        par(mar=c(4,4,3,1))
        powLU <- plotpower(RSE = RSE, effectRange=c(input$minEffect, input$maxEffect),
                           adjustRSE = input$adjustRSEbox, alpha = input$alpha,
                           testtype = input$testtype,
                           targetpower = input$target)
    }
    ## text (110, 10, paste0("RSE = ", round(100*RSE,1), "%") )
})
##############################################################################


output$RSEPlot <- renderPlot({
    ## rotrv$output is an object with class "optimalSpacing" with plot method in secrdesign
    if (rotrv$current){
        par(mar=c(5,6,3,6))      
        # 2025-11-04 plottype changed from "RSE" 
        plot(rotrv$output, plottype = "both", col = "blue", lwd = linewidth, cex = 1.1,
             ylab = expression(paste("Approximate RSE ", hat(italic(D)))))
        ## traffic lights 2019-02-11
        if (input$trafficlightbox) {
            R <- rotrv$output$rotRSE$values$R
            Em <- rotrv$output$rotRSE$values$m
            RSE <- rotrv$output$rotRSE$values$RSE
            arrayspan <- attr(detectorarray(), 'arrayspan') * R
            lights <- trafficlight(arrayspan, input$sigma, Em, RSE)
            symbols(R, y = rep(0,length(R)), inches=FALSE, circles = rep(0.06, length(R)), 
                    bg = trafficcols[lights], add = TRUE, xpd = TRUE)
        }
    }
    else NULL
})
##############################################################################

output$nrmPlot <- renderPlot({
    if (rotrv$current){
        par(mar=c(5,6,3,6))
        plot(rotrv$output, plottype = "nrm", col = "blue", lwd = linewidth, cex = 1.1)
    }
    else NULL
})
##############################################################################

output$costPlot <- renderPlot({
    if (rotrv$current){
        par(mar=c(5,5,3,5))
        cc <- cost(rotrv$output, costs = list(perkm = input$perkm,
                                              perarray = input$perarray,
                                              perdetector = input$perdetector,
                                              pervisit = input$pervisit,
                                              perdetection = input$perdetection))
        maxx <- max(cc$R)
        maxy <- max(cc$totalcost) * 1.2
        par(cex = 1.1)
        defaultargs <- list(x = 0, y = 0, type = "n", las = 1,
                            xlab = expression(paste("Spacing -  ", sigma, "  units")),
                            ylab = paste("Cost", input$currency),
                            ylim = c(0, maxy),
                            xlim = c(0, maxx))
        
        do.call(plot, defaultargs)
        x <- c(cc$R, rev(cc$R))
        y <- c(cc$detrcost, rep(0, nrow(cc)))
        polygon(x, y, col='green')
        y <- c(cc$detrcost + cc$arraycost, rev(cc$detrcost))
        polygon(x, y, col='blue')
        y <- c(cc$detrcost + cc$arraycost + (cc$pathcost + cc$visitcost), 
               rev(cc$detrcost + cc$arraycost))
        polygon(x, y, col='orange')
        y <- c(cc$totalcost, rev(cc$detrcost + (cc$pathcost + cc$visitcost) + cc$arraycost))
        polygon(x, y, col='yellow')
        y <- c(cc$totalcost, rep(0, nrow(cc)))
        polygon(x, y, col=NA)
        legend (x = "top", legend = c("detectors","arrays","travel","detections"),
                fill = c("green","blue","orange","yellow"), horiz = TRUE, cex=0.9)
    }
    else NULL
})
##############################################################################

output$maskPlot <- renderPlot({
    core <- detectorarray()
    msk <- mask()
    par(mar=c(2,2,2,2), xaxs='i', yaxs='i', xpd = input$xpdbox)
    
    if (input$masktype == "Build") {
        if (is.null(core)) return (NULL)
        plot (core, border = input$habxsigma * input$sigma, gridlines = FALSE)
        plot (msk, add = TRUE, col = grey(0.94 - input$dotsbox/5), dots = input$dotsbox)
        plot (core, add = TRUE)
        if (!is.null(habpolyrv$data) && input$polygonbox) {
            plot(habpolyrv$data, add = TRUE)
        }
    }
    else {
        if (!is.null(msk)) {
            if (input$maskcov == 0) {
                cov <- NULL
                plot (msk, col = grey(0.94 - input$dotsbox/5), dots = input$dotsbox,
                      covariate = cov)
            }
            else {
                covar <- covariates(msk)
                D <- as.numeric(covar[,input$maskcov])
                if (any(D<0)) stop ("density must be positive")
                if (input$scaleD)
                    covar[,input$maskcov] <- input$D * D / mean(D, na.rm = T)
                else
                    covar[,input$maskcov] <- D
                covname <- 'D'
                names(covar)[input$maskcov] <- covname
                covariates(msk) <- covar
                plot (msk, dots = input$dotsbox, covariate = covname)
            }
            if (inherits(core, 'traps')) plot (core, add = TRUE)
        }
    }    
    if (!is.null(msk)) {
        if (input$maskedge2) {
            plotMaskEdge(msk, add = TRUE)
        }
        if (!input$xpdbox)
            box()
    }
})
##############################################################################

