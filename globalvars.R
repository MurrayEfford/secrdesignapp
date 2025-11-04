secrversion <- packageVersion('secr')
secrdesignversion <- packageVersion('secrdesign')

publishedversion <- "5.3.0"
publishedyear <- 2025

if (compareVersion(as.character(secrdesignversion), '2.10.0') < 0)
    stop("secrdesignapp 2.0 requires secrdesign version 2.10.0 or later",
         call. = FALSE)

linewidth <- 2  # for various plots 
seconds <- 6   ## default duration for showNotification()
errorduration <- NULL
trafficcols <- c("chartreuse1", "yellow1", "red")

desc <- packageDescription("secrdesign")
summaryfields <- c("date", "time", "note", "detector", "source", "nx", "ny", "spacex", "spacey",
                   "ndetectors", "noccasions", "nrepeats", "distribution", "detectfn", 
                   "D", "lambda0", "sigma", "detperHR", "k", "En", "Er", "Em",
                   "rotRSE", "CF", "route", "cost", "simfn", "model2D", "details", "nrepl", "simtime", 
                   "simRSE", "simRSEse", "simRB", "simRBse", "empiricalRSE")
fieldgroup1 <- 1:17
fieldgroup2 <- 18:36
simfields <- summaryfields[27:36]

availablecores <- min(24, parallel::detectCores())
defaultcores <- max(1, availablecores/2)
defaultrepl <- 10
