## miscellaneous functions


##############################################################################
## miscellaneous functions

## pathlength
## cost
## plotpower    
## plotpowerCI    
## addsequence
## n.eq.r
## nrepeats
## runsims
## runspacing
## readpolygon
## addtosummary
## arraycode
## maskcode
## nrmcode    
## areastr    format area with units
## density    density in animals / ha

##############################################################################

areastr <- function (area) {
  if (area<1000) 
    dec <- 2
  else 
    dec <- 0
  if (isolate(input$areaunit) == "ha") {
    paste0(round(area, dec), " ha")
  }
  else {
    paste0(round(area/100, dec), " km^2")
  }
}
##############################################################################

lengthstr <- function (length, dec) {
  a.unit <- isolate(input$areaunit)
  lth <- if (a.unit == "ha") length else length/1000
  
  if (missing(dec)) {
    if (lth<1000) 
      dec <- 2
    else 
      dec <- 0
  }
  if (a.unit == "ha") {
    paste0(round(lth, dec), " m")
  }
  else {
    paste0(round(lth, dec), " km")
  }
}
##############################################################################

shpfile <- function (filename) {
  if (is.null(filename)) 
    FALSE 
  else {
    if (is.data.frame(filename))
      filename <- filename[,1]
    length(grep(".shp", tolower(filename)))>0
  }
}
##############################################################################

density <- function() {
  ## return density in animals / hectare
  if (input$areaunit == "ha")
    input$D
  else
    input$D/100  ## per sq. km
}
##############################################################################

cost <- function (x, costs = NULL) {
  if (!inherits(x, "optimalSpacing"))
    stop("requires input of class optimalSpacing")
  if (is.null(costs))
    stop("provide costs as list with components 'perkm', 'perarray', ", 
         "'perdetector', 'pervisit', 'perdetection'")
  df <- x$rotRSE$values
  df$C <- apply(df[, c("n","r")],1,sum)
  trps <- attr(x, "traps")
  sig <- attr(x, "detectpar")$sigma
  nocc <- attr(x, "noccasions")
  nrep <- attr(x, "nrepeats")
  
  ## initial path length in sigma units
  pathl <- arraypathlength()
  spc <- spacing(trps); if (is.null(spc)) spc <- NA
  df$pathlength <- df$R * pathl / spc * sig / 1000
  
  df$pathcost  <- costs$perkm * df$pathlength * (nocc+1) * nrep
  df$arraycost <- costs$perarray * nrep
  df$detrcost  <- costs$perdetector * nrow(trps) * nrep
  df$visitcost <- costs$pervisit * nrow(detectorarray()) * (nocc+1) * nrep
  df$detncost  <- costs$perdetection * df$C
  df$totalcost <- apply(df[,c("detrcost","arraycost","pathcost","visitcost","detncost")], 1, sum)
  df
}
##############################################################################

plotpower <- function (RSE = 0.2, effectRange = c(-99,150), testtype = "two.sided",
                       effectIncr = 2, adjustRSE = FALSE, alpha = 0.05,
                       targetpower = 80, col = topo.colors(8)[2], add = FALSE, ...) {
  
  power <- function (D2D1, RSE, adjustRSE, testtype) {
    if (adjustRSE) {
      sdiff <- (log(1 + RSE^2) + log(1 + RSE^2 / D2D1))^0.5
      effect <- log(D2D1) - log(sqrt(1 + RSE^2 / D2D1)) + log(sqrt(1 + RSE^2))
    }
    else {
      sdiff <- (2 * log(1 + RSE^2))^0.5
      effect <- log(D2D1)
    }
    effect <- effect / sdiff
    if (testtype == "two.sided") {
      z.alpha <- qnorm(1-alpha/2)
      pnorm(effect - z.alpha, 0, 1, lower.tail = TRUE) +
        pnorm(-effect - z.alpha, 0, 1, lower.tail = TRUE)
    }
    else if (testtype == "decrease") {
      z.alpha <- qnorm(1-alpha)
      pnorm(-effect - z.alpha, 0, 1, lower.tail = TRUE)
    }
    else {
      z.alpha <- qnorm(1-alpha)
      pnorm(effect - z.alpha, 0, 1, lower.tail = TRUE)
    }
  }
  
  if (!add) {
    xlim <- effectRange
    if (xlim[1] < -98) xlim[1] <- -100
    plot(0,0,type='n', xlim = xlim, ylim=c(0,100), yaxs='i', xaxs = 'i',
         xlab = "", ylab = 'Power %', las=1)
    mtext(side=1, line=2.5, 'Population change %')
  }
  xval <- seq(effectRange[1], effectRange[2], effectIncr)
  ## get critical values
  zero <- which.min(abs(xval))
  dpower <- function (x, target = targetpower) {
    100 * power(x/100+1, RSE, adjustRSE, testtype) - target
  }
  if (100*power(xval[1]/100+1, RSE, adjustRSE, testtype) >= targetpower) {
    lower <- uniroot(dpower, interval = xval[c(1,zero)])$root
    polygon (c(-100,-100,lower, lower), c(0,100,100, 0), col = 'lightgreen')
    text (lower, 105, as.character(round(lower, 1)), cex=0.8, xpd = TRUE)
  }
  else lower <- NA
  if (100*power(xval[length(xval)]/100+1, RSE, adjustRSE, testtype) >= targetpower) {
    upper <- uniroot(dpower, interval = xval[c(zero, length(xval))])$root
    polygon (c(upper, upper, effectRange[2], effectRange[2]), c(0,100,100, 0), col = 'lightgreen')
    text (upper, 105, as.character(round(upper, 1)), cex=0.8, xpd = TRUE)
  }
  else upper <- NA
  
  ## text(x = (par()$usr[3]- par()$usr[1])*0.9, y = 7, testtype, cex=0.9)
  
  powerpct <- 100*power(xval/100+1, RSE = RSE, adjustRSE, testtype)
  lines (xval, powerpct, col = col, lwd = linewidth)
  abline(h = targetpower, lty = 2, xpd = FALSE)
  box()
  list(lower=lower, upper = upper)
}
##############################################################################
preplus <- function(x) paste0(symnum(x, c(-Inf, 0, Inf), c("", "+")), x)

plotpowerCI <- function (RSE = seq(0.05,0.25,0.05), effectRange = c(-99,150), 
                         estimatedRange = effectRange, adjustRSE = FALSE, 
                         alpha = 0.05, effectincr = 2, col = topo.colors(8), plt = TRUE, 
                         add = FALSE, ...) {
  
  powerCI <- function (D2D1, RSE, adjustRSE, alpha) {
    ## effect D2D1 is ratio of final and initial density estimates
    ## RSE is relative standard error of initial density estimate
    ##
    ## find SD and mean of effect size on log scale
    if (adjustRSE) {
      sdiff <- (log(1 + RSE^2) + log(1 + RSE^2 / D2D1))^0.5
      effect <- log(D2D1) - log(sqrt(1 + RSE^2 / D2D1)) + log(sqrt(1 + RSE^2))
    }
    else {
      sdiff <- (2 * log(1 + RSE^2))^0.5
      effect <- log(D2D1)
    }
    ## return back-transformed Wald interval for effect on log scale
    z.alpha <- qnorm(c(alpha/2, 1-alpha/2))
    exp(effect + sdiff * z.alpha)
  }
  
  if (!add) {
    
    xlim <- effectRange
    if (xlim[1] < -98) xlim[1] <- -100
    ylim <- xlim * c(1,1.5)
    plot(0,0,type='n', xlim = xlim, ylim = ylim, yaxs='i', xaxs = 'i',
         xlab = "", ylab = "", las=1)
    mtext (side=1, line=2.5, 'Population change %')
    mtext (side=2, line=3, 'Estimated population change %')
    abline(v=0, lty=2)
    abline(h=0, lty=2)
    abline(0,1, lty=2, col='blue')
    box()
  }
  xval <- seq(effectRange[1], effectRange[2], effectincr)
  nRSE <- length(RSE)
  ci <- array(dim=c(length(xval), 2, nRSE), dimnames = list(xval, c('lower','upper'),RSE))
  for (i in 1:nRSE) {
    ci[,,i] <- t(sapply(xval/100+1, powerCI, RSE = RSE[i], adjustRSE = adjustRSE, alpha = alpha))
    lines(xval, 100*(ci[,1,i]-1), col = col[i], ...)
    lines(xval, 100*(ci[,2,i]-1), col = col[i], ...)
  }
  
  list(RSE = RSE, effectRange = effectRange, adjustRSE = adjustRSE,
       alpha = alpha, limits = ci)
}
##############################################################################

addsequence <- function (trps, routetype) {
  if (tolower(routetype) == "sequential") {
      manualroute$seq <- NULL
      seq <- 1:nrow(trps)
  }
  else {
      if (tolower(routetype) == "manual")
          seq <- manualroute$seq
      else
          seq <- NA
  }
  attr(trps, "seq") <- seq
  trps
}
##############################################################################

nrepeats <- function() {
  if (input$arrayinput=='Region') 1 
  else input$nrepeats
}
##############################################################################

n.eq.r <- function () {
  ## find minimum at n = r
  
  nminr <- function(R) {
    if (arrinput == "Grid") {
      array <- make.grid(nx = input$nx, ny = input$ny, detector = input$detector,
                         spacex = input$spx*R, spacey = input$spy*R,
                         hollow = input$hollow)
    }
    else if (arrinput == "Line") {
      array <- make.grid(nx = input$nline, ny = 1, detector = input$detector,
                         spacing = input$spline*R)
    }
    else if (arrinput == "File") {
      array <- array0
      array[,] <- array[,] * R
    }
    msk <- suppressWarnings(make.mask(array, buffer = input$habxsigma * input$sigma, nx = input$habnx))
    nrm <- Enrm(density(), array, msk, detectpar, input$noccasions, input$detectfn)
    nrm[1] - nrm[2]
  }
  
  arrinput <- isolate(input$arrayinput)
  if (!(arrinput %in% c("Grid", "Line", "File"))) {
    stop ("optimal spacing works only for Grid, Line or File")
  }
  detectpar <- list(lambda0 = input$lambda0, sigma = input$sigma)
  if (nrow(detectorarray()) > 1) {
    if (arrinput == "File") {
      temp <- traprv$scale
      traprv$scale <- 1.0   ## not sure this triggers re-read 2019-02-13
      array0 <- traprv$data
      traprv$scale <- temp
    }
    if (arrinput %in% c("Grid", "Line")) {
      lower <- input$fromR
      upper <- input$toR
    }
    else {
      lower <- 0.01
      upper <- 100
    }
    R <- log_and_run(
        expr = uniroot(nminr, interval = c(lower, upper)),
        "uniroot")
    if (is.null(R) || R$iter==0) {
      showNotification("optimal value not found", type = "error", id = "nooptimum", duration = errorduration)
      return(NA)
    }
    else {
      
      if (arrinput == "Grid") 
        round(R$root * isolate(input$spx),1)
      else if (arrinput == "Line") 
        round(R$root * isolate(input$spline),1)
      else
        return(R$root)
    }
  }
  else NA
}
##############################################################################

readpolygon <- function (fileupload) {
  poly <- NULL
  if (!is.null(fileupload) & is.data.frame(fileupload))
  {
    if (!file.exists(fileupload[1,4])) {
      return(NULL)   ## protect against bad shapefile
    }
    ext <- tolower(tools::file_ext(fileupload[1,1]))
    if (ext == "txt") {
      coord <- read.table(fileupload[1,4])
      if (!is.numeric(coord) || (ncol(coord)<2)) {
        showNotification("bad polygon file(s); clipping switched off",
                         type = "error", id = "nopolyfile", duration = errorduration)
        updateCheckboxInput(session, "polygonbox", value = FALSE)
        return(NULL)
      }
      poly <- secr::boundarytoSF(coord[,1:2])
    }
    else if (ext %in% c("rdata", "rda", "rds")) {
      if (ext == "rds") {
        obj <- readRDS(fileupload[1,4])
      }
      else {
        objlist <- load(fileupload[1,4])
        obj <- get(objlist[1])
      }
      poly <- secr::boundarytoSF(obj)
    }
    else {
      
      if (!(any(grepl(".shp", fileupload[,1])) &&
            any(grepl(".dbf", fileupload[,1])) &&
            any(grepl(".shx", fileupload[,1])))) {
        showNotification("need shapefile components .shp, .dbf, .shx",
                         type = "error", id = "nofile", duration = errorduration)
      }
      else  if (!requireNamespace("sf"))
        showNotification("need package sf to read shapefile", type = "error", id = "nosf", 
                         duration =errorduration)
      else {
        removeNotification(id = "nofile")
        removeNotification(id = "nosf")
        ## not working on restore bookmark 2019-01-24
        dsnname <- dirname(fileupload[1,4])
        ## make temp copy with uniform layername
        file.copy(from = fileupload[,4], 
                  to = paste0(dsnname, "/temp.", tools::file_ext(fileupload[,4])),     
                  overwrite = TRUE)
        poly <- sf::st_read(dsnname)  
      }
    }
  }
  if (inherits(poly, 'sf')) poly <- sf::st_as_sfc(poly)
  poly   
}
##############################################################################

runsims <- function() {
  
  progress <- Progress$new(session, min=1, max=15)
  on.exit(progress$close())
  progress$set(message = 'Simulating...',
               detail = '')
  seed <- input$seed
  if (seed == 0) seed <- NULL
  array <- detectorarray()
  msk <- mask()
  Ndist <- if (input$distributionbtn == 'Poisson') 'poisson' else 'fixed'
  model2D <- input$model2D
  detail <- if (model2D == 'cluster')
    eval(parse(text = paste0("list(", input$details, ")")))
  else NULL
  
  fit <- input$packagebtn %in% c('secr.fit')
  fitargs = list(detectfn = input$detectfn, 
                 method = input$method, 
                 details = list(distribution= tolower(input$distributionbtn)))
  scen <- make.scenarios(
    noccasions = input$noccasions,
    nrepeats = nrepeats(),
    D = density(),
    lambda0 = input$lambda0,
    sigma = input$sigma,
    detectfn = input$detectfn,
    recapfactor = 1)
  
  ## 2019-07-05
  ## vulnerable to 'zero clusters' in sim.popn
  
  sims <- log_and_run(
      expr = run.scenarios (
          nrepl = input$nrepl,
          scenarios = scen,
          trapset = array,
          maskset = msk,
          fit = fit,
          fit.function = if(fit) input$packagebtn else NULL,
          extractfn = summary,
          pop.args = list(Ndist = Ndist, model2D = model2D, details = detail),
          fit.args = if (fit) fitargs else NULL,
          ncores = input$ncores,
          byscenario = FALSE,
          seed = seed,
          terse = TRUE, moves = TRUE, tpa = TRUE),   ## arguments for summary.capthist
      "run.scenarios"
  )
  
  ## empty data.frame 2019-07-02
  counts <- as.data.frame(matrix(nrow = 4,ncol = 3, 
                                 dimnames=list(c('Animals','Detections','Moves','Animals2'),
                                               c('n','mean','se'))))
  if (fit) {
    if (!is.null(sims)) {
      fitsum <- summary(count(sims))
      if (is.list(fitsum))
        counts <- fitsum$OUTPUT[[1]]
    }
  }
  else {  
    sumc <- function(x) {
      c(n = sum(!is.na(x)),
        mean = mean(x, na.rm = TRUE),
        se =  sd(x, na.rm = TRUE)/ sum(!is.na(x))^0.5)
    }
    counts <- t(apply(sims$output[[1]], 2, sumc))
  }
  ## store simulation results in reactive value
  simrv$output <- list(
    nrepl = input$nrepl,
    method = input$method,
    fit = fit,
    proctime = if (is.null(sims)) NA else sims$proctime,
    n = counts['Animals', 'mean'],
    n.se = counts['Animals', 'se'],
    ndet = counts['Detections', 'mean'],
    ndet.se = counts['Detections', 'se'],
    nmov = counts['Moves', 'mean'],
    nmov.se = counts['Moves', 'se'],
    n2 = counts['Animals2', 'mean'],
    n2.se = counts['Animals2', 'se']
  )
  if (fit && !is.null(sims)) {
    predicted <- summary(predict(sims), fields=c('n','mean','se','sd'))$OUTPUT[[1]]
    if (input$method=='none') {
      simrv$output$RB <- NA   
      simrv$output$RBse <- NA   
      simrv$output$empRSE <- NA
    }
    else {    
      simrv$output$RB <- predicted['RB','mean'] * 100
      simrv$output$RBse <- predicted['RB','se'] * 100
      simrv$output$empRSE <- predicted['RB', 'sd'] * 100
    }
    simrv$output$RSE <- predicted['RSE','mean'] * 100
    simrv$output$RSEse <- predicted['RSE','se'] * 100
    simrv$current <- TRUE
    if (input$updateCFbox) {
      CF <- predicted['RSE','mean'] / nrm()$rotRSE
      updateSliderInput(session, "CFslider", value = CF)
    }
  }
  else {
    simrv$output$RSE <- NA   
    simrv$output$RSEse <- NA   
    simrv$output$RB <- NA   
    simrv$output$RBse <- NA   
  }
  simrv$current <- TRUE
  
  if (input$simappendbox) addtosummary() 
  
}

runspacing <- function(sims = FALSE) {
  progress <- Progress$new(session, min=1, max=15)
  on.exit(progress$close())
  R <- seq(input$fromR, input$toR, input$byR)
  if (sims) {
    progress$set(message = 'Simulating RSE for each spacing...')
    rotrv$output <- log_and_run(
        expr = optimalSpacing(
            D = density(),
            traps = detectorarray(),
            detectpar = list(lambda0 = input$lambda0, sigma = input$sigma),
            noccasions = input$noccasions,
            nrepeats = nrepeats(),
            detectfn = input$detectfn,
            fittedmodel = NULL,
            xsigma = input$habxsigma,
            R = R,
            CF = input$CFslider,
            distribution = tolower(input$distributionbtn),
            fit.function = input$packagebtn,
            simulationR = seq(input$fromR, input$toR, input$simbyR),
            nrepl = input$nrepl,
            plt = FALSE,
            ncores = input$ncores,
            method = input$method
        ),
        "optimalSpacing"
    )
  }
  else {
      progress$set(message = 'Approximating RSE for each spacing...')
      rotrv$output <- log_and_run(
          expr = optimalSpacing(
              D = density(),
              traps = detectorarray(),
              detectpar = list(lambda0 = input$lambda0, sigma = input$sigma),
              noccasions = input$noccasions,
              nrepeats = nrepeats(),
              detectfn = input$detectfn,
              fittedmodel = NULL,
              xsigma = input$habxsigma,
              R = R,
              CF = input$CFslider,
              distribution = tolower(input$distributionbtn),
              fit.function = "none",
              plt = FALSE),
          "optimalSpacing"
      )
      
  }
  rotrv$current <- TRUE
}

detectorsource <- function() {
  if (input$arrayinput == 'Region') {
    if (input$layouttype =='Random') {
      if (input$randomtype == 'SRS') "SRS" else "GRTS"
    }
    else if (input$layouttype == 'Systematic') {
      if (input$lacework) "Lacework"
      else if (input$chequerboard) "Chequerboard"
      else "Systematic"
      
    }
    else if (input$layouttype == 'GA') {
      if (input$GAcriterion=='min(n,r)') 'GA min(n,r)' else 'GA n2'
    }
  } 
  else input$arrayinput
}

addtosummary <- function() {
  ## input$fields is character vector of selected fields
  
  ## tentatively suppress 2019-01-18 
  ## invalidateOutputs()
  df <- data.frame(
    date = format(Sys.time(), "%Y-%m-%d"),
    time = format(Sys.time(), "%H:%M:%S"),
    note = if (simrv$current && input$title=="") paste(input$title, "simulated") else input$title,
    detector = input$detector,
    source = detectorsource(),
    nx = if (input$arrayinput=="Grid") input$nx else
      if (input$arrayinput=="Line") input$nline else NA,
    ny = if (input$arrayinput=="Grid") input$ny else NA,
    spacex = if (input$arrayinput=="Grid") input$spx else
      if (input$arrayinput=="Line") input$spline else NA,
    spacey = if (input$arrayinput=="Grid") input$spy else NA,
    ndetectors = nrow(detectorarray()) * nrepeats(),
    noccasions = input$noccasions,
    nrepeats = input$nrepeats,
    distribution = input$distributionbtn,
    detectfn = input$detectfn,
    
    D = density(),
    lambda0 = input$lambda0,
    sigma = input$sigma,
    detperHR = nrm()$detperHR,
    k = if (input$detectfn=="HHN") round(density()^0.5 * input$sigma / 100,3) else NA,
    En = round(nrm()$En,1),
    Er = round(nrm()$Er,1),
    Em = round(nrm()$Em,1),
    rotRSE = round(nrm()$rotRSE * input$CFslider * 100, 1),
    CF = round(input$CFslider, 3),
    # perkm = input$perkm,
    # perarry = input$perarray,
    # perdetr = input$perdetector,
    # pervist = input$pervisit,
    # perdetn = input$perdetection,
    
    route = round(arraypathlength()/1000 * nrepeats(),3),
    cost = round(nrm()$totalcost, 2)
  )
  
  newfields <- ""        
  
  simdf <- data.frame(
    simfn = "",
    model2D = "",
    details = "",
    nrepl = NA,
    simtime = NA,
    simRSE = NA, 
    simRSEse = NA,
    simRB = NA,
    simRBse = NA,
    empiricalRSE = NA)
  
  if (simrv$current && !is.null(simrv$output$fit)) {
    simdf$simfn <- input$packagebtn
    simdf$model2D <- input$model2D
    simdf$details <- input$details
    simdf$nrepl <- input$nrepl
    simdf$simtime <- round(simrv$output$proctime,2)
    if (simrv$output$fit) {
      simdf$simRSE <- simrv$output$RSE
      simdf$simRSEse <- simrv$output$RSEse
      simdf$simRB <- simrv$output$RB
      simdf$simRBse <- simrv$output$RBse
      simdf$empiricalRSE <- simrv$output$empRSE
    }
    newfields <- unique(c(isolate(input$fields2), c("simfn", "model2D", "nrepl", "simtime") ))
  }
  
  df <- cbind(df, simdf)
  sumrv$value <- rbind (sumrv$value, df)
  # updateCheckboxGroupInput("fields2", selected = newfields)  ## FAILS 2019-01-23
  rownames(sumrv$value) <- paste0("Scenario", 1:nrow(sumrv$value))
}
##############################################################################

maskOK <- function () {
  if (is.null(detectorarray())) return (TRUE)
  if (input$masktype == 'Build' && !is.null(habpolyrv$data)) {
    sum(pointsInPolygon(detectorarray(), habpolyrv$data)) > 0
  }
  else if (input$masktype == 'File' && !is.null(maskrv$data)) {
    ## "If poly is a mask object then its cells must be aligned
    ##  to the x- and y- axes"
    sum(pointsInPolygon(detectorarray(), maskrv$data)) > 0
  }
  else TRUE
}
##############################################################################

## log_and_run

#-------------------------------------------------------------------------------
# Function to wrap and log an expression
# It must be defined where it can access or be passed the reactiveValues object
log_and_run <- function(expr, callstr = "") {
  if (callstr != "") {
    if (substring(callstr, nchar(callstr), nchar(callstr)) != "\n") callstr <- paste0(callstr, "\n")
    shiny::isolate({
      log_data$messages <- c(log_data$messages, paste0("CALL: [", trunc(Sys.time()), "]: ", callstr))
    })
  }
  tryCatch (
    withCallingHandlers( 
      expr,
      message = function(m) {
        shiny::isolate({
          log_data$messages <- c(log_data$messages, paste0("MESSAGE: [", trunc(Sys.time()), "]: ", m$message))
          invokeRestart("muffleMessage")
        })
      },
      warning = function(w) {
        shiny::isolate({
          log_data$messages <- c(log_data$messages, paste0("WARNING [", trunc(Sys.time()), "]: ", w$message, "\n"))
          invokeRestart("muffleWarning")
        })
      },
      error = function(e) {
        shiny::isolate({
          log_data$messages <- c(log_data$messages, paste0("ERROR [", trunc(Sys.time()), "]: ", e$message, "\n"))
        })
        # Errors halt execution unless caught by tryCatch
      }
    ),
    error = function(e) return(NULL)
  )
}
#-------------------------------------------------------------------------------

# number of detectors in lacework design
Klace <- function (A, a, b, radius = NULL) {
  k <- A /a^2 * (2 * a/b - 1)
  if (!is.null(radius)) { ## approximate only
    radius <- min(radius, a/2)
    k <- k * (4 * trunc(radius/b) + 1) / (4 * trunc(a/2/b) + 1)
  }
  k
}
#-------------------------------------------------------------------------------

