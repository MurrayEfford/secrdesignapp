## reactiveValues

## simrv, rotrv, RSErv, pxyrv : logical
## poprv, optrv
## sumrv : summary table
## current, selecting, 
## manualroute
## traprv, regionrv, exclusionrv, habpolyrv, maskrv, edetrv
## log_data

##############################################################################

simrv <- reactiveValues(current = FALSE, output = NULL)
rotrv <- reactiveValues(current = FALSE, output = NULL)
RSErv <- reactiveValues(current = FALSE, value = NULL, adjRSE = NULL)
pxyrv <- reactiveValues(current = FALSE, xy = NULL, value = NULL)
poprv <- reactiveValues(v = 0)  # used to invalidate and re-plot popn
# arrrv <- reactiveValues(v = 0)  # used to invalidate and re-plot detectorarray
optrv <- reactiveValues(value = NULL, message = "", OK = TRUE)  
manualroute <- reactiveValues(seq = NULL)
current <- reactiveValues(unit = "ha")
selecting <- reactiveValues(v=FALSE)
sumrv <- reactiveValues(
    value = read.csv(text = paste(summaryfields, collapse = ", "))
)
# accumulate log messages
log_data <- reactiveValues(messages = character(0))

##############################################################################

## using advice of Joe Cheng 2018-03-23 to allow resetting of fileInputs
## https://stackoverflow.com/questions/49344468/resetting-fileinput-in-shiny-app

traprv <- reactiveValues(
    data = NULL,
    clear = FALSE,
    scale = 1.0
)

regionrv <- reactiveValues(
    data = NULL,
    area = NULL,
    clear = FALSE
)

exclusionrv <- reactiveValues(
    data = NULL,
    clear = FALSE
)

habpolyrv <- reactiveValues(
    data = NULL,
    clear = FALSE
)

maskrv <- reactiveValues(
    data = NULL,
    clear = FALSE
)

edetrv <- reactiveValues(
    ntraps = NA,
    alltrapsmask = NULL,
    clear = FALSE
)

