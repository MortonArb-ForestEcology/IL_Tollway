library(rgdal)


tollway.trees <- readOGR("data/shapefiles/I355 Tree_Shrub Shapefiles_DRAFT_02172017/TREES.shp")
summary(tollway.trees)
plot(tollway.trees)

tollway.labels <- readOGR("data/shapefiles/I355 Tree_Shrub Shapefiles_DRAFT_02172017/LABELS.shp")
summary(tollway.labels)
plot(tollway.labels)


# Zoom in on bottom right
bbox.trees <- bbox(tollway.trees)

# Getting the range and then using that to help zoom in on the bottom right-hand size
xrange <- bbox(tollway.trees)[1,2]-bbox(tollway.trees)[1,1]
yrange <- bbox(tollway.trees)[2,2]-bbox(tollway.trees)[2,1]
plot(tollway.trees, xlim=c(bbox.trees[1,2]-xrange/5, bbox.trees[1,2]), ylim=c(bbox.trees[2,1], bbox.trees[2,1]+yrange/10))

png("Tollway_Example.png", height=6, width=4, units="in", res=400)
plot(tollway.trees, xlim=c(bbox.trees[1,2]-xrange/90, bbox.trees[1,2]-(xrange/120)), ylim=c(bbox.trees[2,1], bbox.trees[2,1]+yrange/155))
plot(tollway.labels, xlim=c(bbox.trees[1,2]-xrange/90, bbox.trees[1,2]-(xrange/120)), ylim=c(bbox.trees[2,1], bbox.trees[2,1]+yrange/155), add=T)
dev.off()

plot(tollway.trees[tollway.trees$Layer=="Landscaping_Trees",], xlim=c(bbox.trees[1,2]-xrange/90, bbox.trees[1,2]-(xrange/180)), ylim=c(bbox.trees[2,1], bbox.trees[2,1]+yrange/155))
plot(tollway.labels, xlim=c(bbox.trees[1,2]-xrange/90, bbox.trees[1,2]-(xrange/180)), ylim=c(bbox.trees[2,1], bbox.trees[2,1]+yrange/155), add=T)


bbox.mow <- bbox(tollway.trees[tollway.trees$Layer=="Landscaping_Mowline",])
plot(tollway.trees[tollway.trees$Layer=="Landscaping_Mowline",])
plot(tollway.labels, xlim=c(bbox.trees[1,2]-xrange/90, bbox.trees[1,2]-(xrange/180)), ylim=c(bbox.trees[2,1], bbox.trees[2,1]+yrange/155), add=T)


summary(tollway.trees[tollway.trees$Layer=="Landscaping_Shrubs",])
plot(tollway.trees[tollway.trees$Layer=="Landscaping_Shrubs",], xlim=c(bbox.trees[1,2]-xrange/90, bbox.trees[1,2]-(xrange/180)), ylim=c(bbox.trees[2,1], bbox.trees[2,1]+yrange/155))
