output$maskPrint <- renderPrint({
    if (is.null(mask()))
        cat("No mask yet\n")
    else 
        summary(mask())
})
##############################################################################

# Render the log content
output$logPrint <- renderPrint({
    if (length(log_data$messages) > 0) {
        cat(paste(log_data$messages, collapse = ""))
    } else {
        cat("No calls, messages, warnings, or errors captured.")
    }
})
##############################################################################
