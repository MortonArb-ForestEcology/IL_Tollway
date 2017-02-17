# Attempt to convert the polylines to polygons with attribute info
library(rgdal)
library(rgeos)

# Workflow steps
# 1. read in the shapefile
# 2. convert lines to polygons
# 3. calculate polygon size; below certain theshold is a "tree"
# 4. Determine whether polygon contains "trees" or not
#    - if two big polygons overlap (e.g. polygons 1 & 2, just remove 1 of them)
# 5. Assign planting type based on resulting polygon
#    a. no holes/sub polygons = "dense"
#    b. if there are holes or overlapping polygons = scattered
# 4. remove trees so we're just looking at patches
# 5. Find a way to convert text from the "labels" file to species info

# Read in the data with spatial information
tollway.trees <- readOGR("data/shapefiles/I355 Tree_Shrub Shapefiles_DRAFT_02172017/TREES.shp")
summary(tollway.trees)


# Plot a segment or two as a test
bbox.trees <- bbox(tollway.trees)
xrange <- bbox.trees[1,2]-bbox.trees[1,1]
yrange <- bbox.trees[2,2]-bbox.trees[2,1]
plot(tollway.trees[tollway.trees$Linetype=="Continuous",], pch=19, cex=0.5, xlim=c(bbox.trees[1,2]-xrange/90, bbox.trees[1,2]-(xrange/120)), ylim=c(bbox.trees[2,1], bbox.trees[2,1]+yrange/155))



# 2. gPolygonize in the rgeos packages does a decent job of making some polyons (but we'll lose some info)
# We can't really tell different plantings apart this way, but we can see their holes, which will let \
# us differentiate "dense" vs. "scattered"
# Note: each polygon is a separate object now in test.poly
test.poly <- gPolygonize(tollway.trees)
summary(test.poly)
plot(test.poly, xlim=c(bbox.trees[1,2]-xrange/90, bbox.trees[1,2]-(xrange/120)), ylim=c(bbox.trees[2,1], bbox.trees[2,1]+yrange/155))


plot(tollway.trees, xlim=c(bbox.trees[1,2]-xrange/10, bbox.trees[1,2]-(xrange/15)), ylim=c(bbox.trees[2,1] + yrange/150, bbox.trees[2,1]+yrange/50))
plot(test.poly, xlim=c(bbox.trees[1,2]-xrange/10, bbox.trees[1,2]-(xrange/15)), ylim=c(bbox.trees[2,1] + yrange/150, bbox.trees[2,1]+yrange/50))





# 3. determine which polygons contain others (i.e. is sparse plantings)
summary(test.poly[1:100])
plot(tollway.trees, xlim=c(1084082, 1087800), ylim=c(1773900, 1776392))
plot(test.poly, xlim=c(1084950, 1087800), ylim=c(1773900, 1776392))
# plot(test.poly[1:100])

plot(test.poly[1:300])

test.contain <- gContains(test.poly[1:300], byid=T)
summary(test.contain)
