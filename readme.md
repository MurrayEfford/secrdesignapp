# secrdesignapp 1.0

secrdesignapp is a partial interactive interface to the R package [secrdesign 2.5.5](https://CRAN.R-project.org/package=secrdesign). See the Help tab for details.

Click this link to run the app in your web browser from a University of Otago server:

[secrdesignapp](https://www.stats.otago.ac.nz/secrdesignapp)

To run in a local R session directly from GitHub:

```r
library(shiny)
runGitHub("secrdesignapp", "MurrayEfford")
```

Simulations may run faster on your own machine, and the GitHub version is always the latest.

----

Some test data are provided:

| File name | Description | Usage |
|--------|-------------------------------|------------------|
ovtrap.txt | Locations of possum traps in the Orongorongo Valley | Design - File input of detector array |
OVforest1.dbf | ESRI polygon shapefile extent of habitat near possum traps | Options - Habitat clip to polygons |
OVforest1.shp |||
OVforest1.shx |||
