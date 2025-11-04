## rendertable

##############################################################################

output$summarytable <- renderTable({
    fields <- c(input$fields1, input$fields2)
    tmp <- t(sumrv$value[,fields])
    if (ncol(tmp)>0) colnames(tmp) <- paste0('Scenario', 1:ncol(tmp))
    tmp <- cbind(Field = fields, tmp)
    tmp } , spacing = "xs"
)

##############################################################################
