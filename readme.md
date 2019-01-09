# secrdesignapp 1.1

secrdesignapp is a partial interactive interface to the R package [secrdesign 2.5.5](https://CRAN.R-project.org/package=secrdesign). See the Help tab for details.

Version 1.1 is in progress. The main new feature is support for systematic and random arrays within study area polygon(s). The help has yet to be updated.

Click this link to run secrdesignapp 1.0 in your web browser from a University of Otago server:

[secrdesignapp](https://www.stats.otago.ac.nz/secrdesignapp)

To run secrdesignapp 1.1 in a local R session directly from GitHub:

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
OVforest.dbf | ESRI polygon shapefile extent of habitat near possum traps | Design - Region and Options - Habitat clip to polygons |
OVforest.shp |||
OVforest.shx |||
regionxy.txt | text file of a hypothetical study area boundary |Design - Region and Options - Habitat clip to polygons|
