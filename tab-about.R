tababout <- tabPanel("About", value = "about_tab",
         h2("secrdesign app 2.0"), br(),
         
         h5(paste("This Shiny application provides an interface to the R package 'secrdesign', version", 
                  publishedversion, ".")),
         br(),
         h5("Copyright 2019, 2022, 2025 Murray Efford"),
         h5("The application is released under the"),
         a("GNU General Public License Version 3.0", href="https://www.gnu.org/licenses/gpl-3.0.txt", target="_blank"), br(),
         br(),
         h5("For further information see "), 
         a("The SECR Book", href="https://murrayefford.github.io/SECRbook/", target="_blank"), br(),
         a("www.otago.ac.nz/density", href="https://www.otago.ac.nz/density", target="_blank"), br(),
         a("CRAN.R-project.org/package=secrdesign", href="https://CRAN.R-project.org/package=secrdesign", target="_blank"), br(),
         a("https://github.com/MurrayEfford/secrdesignapp", href="https://github.com/MurrayEfford/secrdesignapp", target="_blank"), br(),
         br(),
         
         h5("Citation"),
         h5("Efford, M. G. and Boulanger, J. (2019) Fast evaluation of study designs for spatially explicit capture-recapture. "),
         a("Methods in Ecology and Evolution 10, 1529-1535. ", href="https://doi.org/10.1111/2041-210X.13239", target ="_blank")
         
)