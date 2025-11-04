setBookmarkExclude(c("simulatebtn", "simulatebtn2", "spacingbtn", "appendbtn",
                     "clearallbtn", "clearlastbtn", "selectnonebtn", "selectallbtn",
                     "suggestbtn", "suggestlinebtn", "suggestfilebtn",
                     "resetbtn", "routebtn", "randompopbtn", 
                     # "randomarraybtn", 
                     "selectfieldsbtn", "selecting"))

# Save extra values in state$values when we bookmark
onBookmark(function(state) {
    state$values$simrvoutput <- simrv$output     # does not work 
    state$values$sumrv <- sumrv$value            # works
    state$values$manualroute <- manualroute$seq
    state$values$port <- session$clientData$url_port
    ## can manually recover with e.g.
    ## readRDS('d:/density secr 3.2/secrdesignapp/shiny_bookmarks/9c88715bacc260cf/values.rds')$port
})    
# Read values from state$values when we restore
onRestore(function(state) {
    simrv$output <- state$values$simrvoutput
    sumrv$value <- state$values$sumrv
    current$unit <- input$areaunit
    manualroute$seq <- state$values$manualroute
    updateNumericInput(session, "D", paste0("D (animals / ", input$areaunit, ")"))
})
##############################################################################
