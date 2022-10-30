# R code to generate main results
library(secrdesign)

# array type : Grid
array <- make.grid(nx = 8, ny = 8, spacex = 20, spacey = 20,
    detector = 'proximity', hollow = FALSE)

mask <- make.mask (array, buffer = 120, nx = 32, type = 'trapbuffer')

scen <- make.scenarios(trapsindex = 1, noccasions = 5, nrepeats = 1,
    D = 5, sigma = 30, lambda0 = 0.2, detectfn = 'HHN')

scensum <- scenarioSummary(scen, trapset = array, mask = mask, CF = 1)

# scensum is a dataframe with one row and columns for En, Er etc.
# see ?scenarioSummary for details