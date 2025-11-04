# 

load(system.file('example/TostExample.RData', package = 'secrdesign'))
# "mask_df"     "bigmask_df"  "alltraps_df"
msk <- read.mask(data = alltraps_df[,1:2])
Tost_outline <- st_union(gridCells(msk))
plot(Tost_outline, add = TRUE)
saveRDS(Tost_outline, file = 'Tost_outline.RDS')