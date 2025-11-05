
getSPcode <- function (inputfilename, varname, apply = TRUE) {
    filename <- inputfilename[1,1]
    if (is.null(filename) || !apply) {
        return("")
    }
    else {
        ext <- tolower(tools::file_ext(filename))
        if (ext == "txt") {
            code <- paste0( 
                "# coordinates from text file\n",
                "coord <- read.table('", filename, "')   # read boundary coordinates\n",
                varname, " <- secr::boundarytoSF(coord)  # convert to Simple Features\n")
        }
        else if (ext %in% c("rdata", "rda")) {
            objlist <- load(inputfilename[1,4])
            code <- paste0( 
                "# sf from RData file\n",
                "objlist <- load('", filename, "')\n",
                varname, " <- get(objlist[1]) \n")
        }
        else if (ext == "rds") {
            code <- paste0( 
                "# sf from RDS file\n",
                varname, " <- readRDS('", filename, "') \n")
        }
        else {
            code <- paste0(
                "# ESRI polygon shapefile\n",
                varname, " <- sf::st_read('", 
                tools::file_path_sans_ext(basename(filename)), 
                ".shp')\n"
            )
        }
        code
    }
}    

arraycode <- function (comment = FALSE) {
    # returns the R code needed to generate the specified array, 
    # as a character value
    if (is.null(detectorarray())) {
        code <- ""  
    }
    else {
        if (input$arrayinput == "Grid") {
            code <- paste0(
                "array <- make.grid(",
                "nx = ", input$nx, ", ",
                "ny = ", input$ny, ", ",
                "spacex = ", input$spx, ", ",
                "spacey = ", input$spy, ",\n    ",
                "detector = '", input$detector, "', ",
                "hollow = ", input$hollow, ")\n"
            )
        }
        
        else if (input$arrayinput == "Line") {
            code <- paste0(
                "array <- make.grid(",
                "nx = ", input$nline, ", ",
                "ny = 1, ",
                "spacing = ", input$spline, ", \n    ",
                "detector = '", input$detector, "')\n"
            )
        }
        
        else if (input$arrayinput == "File") {
            
            trapargs <- input$trapargs
            if (trapargs != "")
                trapargs <- paste0(", ", trapargs)
            code <- paste0("array <- read.traps ('", 
                           input$trapfilename[1,"name"],
                           "', detector = '", input$detector, "'", trapargs, ")\n")
            # if (input$scalefactor != 1.0) {
            #     code <- paste0(code, 
            #                    "# optional scaling about centroid\n",
            #                    "meanxy <- apply(array,2,mean)\n",
            #                    "array[,1] <- (array[,1]- meanxy[1]) * ", input$scalefactor, " + meanxy[1]\n",
            #                    "array[,2] <- (array[,2]- meanxy[2]) * ", input$scalefactor, " + meanxy[2]\n")
            #     #"array[,] <- array[,] * ", input$scalefactor, "\n")
            # }
        }
        
        else if (input$arrayinput == "Region") {
            
            regioncode <- getSPcode(input$regionfilename, "region", TRUE)
            excludedcode <- getSPcode(input$exclusionfilename, "excluded", input$exclusionbox)
            
            
            if (input$randomorigin) 
                origincode <- "NULL"
            else {
                origincode <- paste0("sf::st_bbox(region)[1:2] + ", input$sppgrid/2)
            }
            
            if (input$chequerboard) { 
                chequercode <- paste0(",\n    chequerboard = 'white'")
            }
            else {
                chequercode <- ""  # default
            }
            
            
            if (input$clustertype == "Grid") {
                clustercode <- paste0("cluster <- make.grid(nx = ", input$nx, ", ny = ", input$ny, 
                                      ", detector = '", input$detector, "', \n",
                                      "    spacex = ", input$spx, ", spacey = ", input$spy, ", ",
                                      "hollow = ", input$hollow, ")\n")
            }
            else if (input$clustertype == "Line") {
                clustercode <- paste0("cluster <- make.grid(nx = ", input$nline, 
                                      ", ny = 1, detector = '", input$detector, "', \n",
                                      "    spacing = ", input$spline, ")\n")
            }
            else if (input$clustertype == "File") {
                trapargs <- input$trapargs
                if (trapargs != "")
                    trapargs <- paste0(", ", trapargs)
                clustercode <- paste0("cluster <- read.traps ('",
                                      input$trapfilename[1,"name"],
                                      "', detector = '", input$detector, "', ", trapargs, ")\n")
            }
            else {
                if (input$layouttype == "Systematic")
                    clustercode <- paste0("cluster <- make.grid(nx = 1, ny = 1, detector = '", 
                                          input$detector, "')\n")
                else
                    clustercode <- ""
            }
            
            if (input$clustertype %in% c("Grid", "Line", "File")) {
                if (input$chequerboard)
                    edgemethodcode <- paste0(", edgemethod = '", input$edgemethod, "'")
                else
                    edgemethodcode <- paste0(",\n    edgemethod = '", input$edgemethod, "'")
                
                if (input$rotation != 0)
                    rotatecode <- paste0("cluster <- rotate(cluster, ", input$rotation, ")\n")
                else 
                    rotatecode <- ""
            }
            else {
                edgemethodcode <- ""
                rotatecode <- ""
            }
            
            if (excludedcode != "") {
                exclusioncode <- ", exclude = excluded"
                if (input$edgemethod == "allinside")
                    exclusioncode <- c(exclusioncode, ", exclmethod = 'alloutside'")
                else if (input$edgemethod == "anyinside")
                    exclusioncode <- c(exclusioncode, ", exclmethod = 'anyoutside'")
            }
            else
                exclusioncode <- ""
            
            if ((input$layouttype == "Random" || input$randomorigin) 
                & input$seedpgrid>0) {
                seedcode <- paste0("set.seed(", input$seedpgrid, ")\n")
            }
            else {
                seedcode <- ""
            }
            
            if (input$layouttype == "Systematic") {
                
                if (input$lacework && input$clustertype == "Single detector") {
                    code <- paste0( 
                        regioncode,
                        excludedcode,
                        seedcode,
                        "array <- make.lacework(region = region, ",
                        "detector = '", input$detector, "', \n    ", 
                        "spacing = c(", input$splace*input$sppgrid, ", ", input$sppgrid,
                        "), rotate = ", input$rotation, ")\n")
                }
                else {
                    code <- paste0( 
                        regioncode,
                        excludedcode,
                        clustercode,
                        rotatecode,
                        seedcode,
                        "array <- make.systematic(spacing = ", input$sppgrid, ", ",
                        "region = region, \n    ", 
                        "cluster = cluster, origin = ", origincode, chequercode, edgemethodcode, 
                        exclusioncode, ")\n")
                }
            }
            else if (input$layouttype == "Random") {
                if (input$clustertype %in% c("Grid", "Line", "File"))
                    clusterarg <- ",\n    cluster = cluster"
                else 
                    clusterarg <- ""
                
                code <- paste0( 
                    regioncode,
                    excludedcode,
                    clustercode,
                    rotatecode,
                    seedcode,
                    "array <- trap.builder(n = ", input$numpgrid, ", ",
                    "method = '", input$randomtype, "', ",
                    "region = region", clusterarg, 
                    edgemethodcode, exclusioncode, ")\n")
            }
            else if (input$layouttype == "GA") {
                detectparcode <- paste0("list(lambda0 = ", input$lambda0, ", sigma = ", input$sigma, ")")
                mskcode <- ""
                alltrapscode <- ""
                code <- paste0( 
                    regioncode,
                    mskcode,
                    alltrapscode,
                    "array <- GAoptim(mask = msk, alltraps = alltraps", 
                    ", ntraps = ", input$numpgrid, 
                    ", detectpar = ", detectparcode,
                    ", noccasions = ", input$noccasions, 
                    ", detectfn = ", input$detectfn, 
                    ", D = ", input$D, 
                    ", criterion = ", match(input$GAcriterion, c('min(n,r)','n2'))+3, 
                    ", penalty = NULL",
                    ", seed = ", if (input$seedpgrid == 0) "NULL" else input$seedpgrid,
                    ", ngen = ", input$GAngen, 
                    ", popsize = ", input$GApopsize, 
                    ")\n")
            }
            else stop ("unknown layouttype")
            
        }
        else stop ("unknown arrayinput")
        
        if (comment) {
            tmp <- lapply(strsplit(code, "\n")[[1]], function(x) paste0("# ", x))
            tmp$sep <- "\n"
            code <- do.call(paste, tmp)
        }
    }
    code        
}
##############################################################################

maskcode <- function (arrayname) {
    if (input$masktype == 'Build') {
        type <- if (input$maskshapebtn == 'Rectangular') 'traprect' else 'trapbuffer'
        buffer <- as.character(round(input$habxsigma * input$sigma,2))
        
        polycode <- ""
        polyhabitat <- ""
        
        if (input$polygonbox && !is.null(habpolyrv$data)) { 
            polyhabitat <- input$includeexcludebtn == "Include"
            polycode <- getSPcode(input$habpolyfilename, "poly", input$polygonbox)
        }
        paste0(polycode,
               "mask <- make.mask (", arrayname, 
               ", buffer = ", buffer, 
               ", nx = ", input$habnx, 
               ", type = '", type, "'",  
               if (polycode == "") "" else ",\n    poly = poly",
               if (polycode == "") "" else ", poly.habitat = ", polyhabitat,
               ")\n")
    }
    else {
        maskargs <- input$maskargs
        if (maskargs != "")
            maskargs <- paste0(", ", maskargs)
        
        if (!is.null(input$maskfilename))
            paste0("mask <- read.mask ('", input$maskfilename[1,1], maskargs, "')\n")
    }
}
##############################################################################

nrmcode <- function() {
    pathl <- as.character(arraypathlength()/1000)
    detfn <- input$detectfn
    if (is.character(detfn)) detfn <- paste0("'", detfn, "'")
    costcode <- if (nrm()$totalcost==0) ""
    else paste0(
        ",\n    unitcost = list(perkm = ", input$perkm, ", ",
        "perarray = ", input$perarray, ", ",
        "perdetector = ", input$perdetector, ",\n",
        "                    pervisit = ", input$pervisit, ", ",
        "perdetection = ", input$perdetection, ")",
        ",\n    routelength = ", pathl,  ", setupoccasion = ", input$setupoccasion, ", costing = TRUE")
    paste0(
        "# R code to generate main results\n",
        "library(secrdesign)\n\n",
        "# array type : ", input$arrayinput, "\n",
        arraycode(),
        "\n",
        maskcode("array"),
        "\n",
        "scen <- make.scenarios(trapsindex = 1, noccasions = ", input$noccasions, ", ", 
        "nrepeats = ", nrepeats(), ",\n    D = ", density(), ", sigma = ", input$sigma, ", ",
        "lambda0 = ", input$lambda0, ", detectfn = ", detfn, ")\n\n",
        
        "scensum <- scenarioSummary(scen, trapset = array, mask = mask, CF = ", 
        round(input$CFslider, 3), 
        costcode, ")\n\n",
        "# scensum is a dataframe with one row and columns for En, Er etc.\n",
        "# see ?scenarioSummary for details")
}
##############################################################################
