## downloadhandler

##############################################################################

output$downloadSummary <- downloadHandler(
    filename = "summary.csv",
    content = function(file) {
        write.csv(sumrv$value, file, row.names = TRUE)
    }
)

output$downloadSummaryrds <- downloadHandler(
    filename = "summary.rds",
    content = function(file) {
        saveRDS(sumrv$value, file)
    }
)

output$downloadArray <- downloadHandler(
    filename = "array.txt",
    content = function(file) {
        head <- paste0("\n", arraycode(comment = TRUE))
        write.traps(detectorarray(), header = head, file)
    }
)

output$downloadSpacing <- downloadHandler(
    filename = "spacing.RData",
    content = function(file) {
        spacingOutput <- rotrv$output
        save(spacingOutput, file = file)
    }
)
output$downloadnrmcode <- downloadHandler(
    filename = "nrmcode.R",
    content = function(file) {
        nrmtext <- nrmcode()
        cat(nrmtext, file = file)
    }
    , contentType = "text/R"
)
