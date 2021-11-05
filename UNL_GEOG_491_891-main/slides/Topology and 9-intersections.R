
library(tidyverse)
library(GISTools)
library(sf)
library(tmap)
library(raster)

counties <- sf::read_sf("./data/CBW/County_Boundaries.shp") %>% sf::st_make_valid()
dams <- sf::read_sf("./data/CBW/Dam_or_Other_Blockage_Removed_2012_2017.shp") %>% sf::st_make_valid()
streams <- sf::read_sf("./data/CBW/Streams_Opened_by_Dam_Removal_2012_2017.shp") %>% sf::st_make_valid()

# counties in PA
pa.counties <- counties %>% filter(STATEFP10 == 42)

# methods to determine dams in PA
pa.dams <- st_intersection(dams, pa.counties)
# or
pa.dams <- dams[pa.counties,]

# Boolean result
st_intersects(dams, pa.counties)

# order matters
dams %>% st_intersects(x = ., y = pa.counties)
dams %>% st_intersects(x = pa.counties, y = .)

# get a dense logical matrix
dams %>% st_intersects(x = ., y = pa.counties, sparse = F)

# Disjoint
dams %>% st_disjoint(., pa.counties, sparse = F)
# Within
dams %>% st_within(., pa.counties, sparse = F)

# More examples
streams.tioga %>% st_covered_by(., c.tioga)
tm_shape(c.tioga) + tm_polygons() + tm_shape(streams.tioga) + tm_lines(col = "blue")

# distance
streams.tioga %>% st_is_within_distance(., dams, 1)

###
# TMDL Regulations - Total Maximum Daily Loads (kg/yr) - concentration or raw scale

## Run-off in VT
# NHDs = National Hydrographic Dataset (big data models - how we quantify hydrologic units across the U.S.)
nhds <- sf::read_sf("./data/nhdplus_loads.shp") %>% sf::st_make_valid()
glimpse(nhds)
# manually set the number of classes with n = 10
tm_shape(nhds) + tm_polygons("Baseline_L", n = 10)
# Baseline_L is the phosphorus load in kg/year

# RPCs = Regional Planning Commissions (roughly correspond to counties)
rpcs <- sf::read_sf("./data/gn_vt_rpcs.shp") %>% sf::st_make_valid()
glimpse(rpcs)
tm_shape(rpcs) + tm_polygons(col = "INITIALS")

## Overlay the NHDs and RPCs
tm_shape(rpcs) + tm_borders(col = "red") +
  tm_shape(nhds) + tm_polygons(col = "Baseline_L", n = 7) +
  tm_shape(rpcs) + tm_borders(col = "red")

## How to calculate total phosphorus load in each RPC?

# do the join
nhd_rpcs <- st_join(nhds, rpcs, join = st_intersects)
# look at it/confirm it worked
glimpse(nhd_rpcs)
# plot it - shade them by RPC value
tm_shape(nhd_rpcs) + tm_polygons(col = "RPC")

## group_by RPC and summarize areas
nhd_rpcs %>%
  group_by(RPC) %>%
  summarize(totalLoad = sum(Baseline_L))

## Add a line to plot it - with the pipe!
nhd_rpcs %>%
  group_by(RPC) %>%
  summarize(totalLoad = sum(Baseline_L)) %>%
  tm_shape(.) + tm_polygons(col = "totalLoad") # <- this line is new

## Lovelace Method
# using aggregate instead
aggregate(x = nhds, by = rpcs, FUN = sum) # throws an error... what's the problem?
# names are characters - 3 total character attributes
glimpse(nhds)

# fix the problem - use select to get rid of characters, then aggregate
agg.rpcs <- nhds %>% dplyr::select(-SOURCEFC, -NHDPlus_Ca, -Tactical_B) %>%
  aggregate(x = ., by = rpcs, FUN = sum)

# plot it
tm_shape(agg.rpcs) + tm_polygons(col = "Baseline_L")

## Issues with overlap of NHDs across RPCs?
nhd_rpcs %>% group_by(NHDPlus_ID) %>% summarise(count = n()) %>%
  arrange(desc(count))
# Need to do area-weighted interpolation! To figure out proportion of NHD (and load) in each of the RPCs

# area-weighted interpolation
# Start with ndhs and just pick load/geometry (so it doesn't do the math on every single variable)
interp.loads <- nhds %>% dplyr::select(Baseline_L, geometry) %>%
  # aw = area-weighted - interpolate across the RPCs
  st_interpolate_aw(., rpcs, extensive = T)
# Plot
tm_shape(interp.loads) + tm_polygons(col = "Baseline_L")
# How do we quantify how different these methods are?

# do another join! Between the output of the right and the wrong methods
comparison <- st_join(agg.rpcs, interp.loads, st_equals) # st_equals instead of st_intersect because the geometry is equal
# calculate the error, then map it
tmap_mode("view")
comparison %>% mutate(diff = Baseline_L.x - Baseline_L.y) %>%
  tm_shape(.) + tm_polygons(col = "diff") +
  tm_shape(nhds) + tm_borders(col = "blue")


####################
# Raster exercise
myras <- raster::raster("./data/ts_2016.1007_1013.L4.LCHMP3.CIcyano.MAXIMUM_7day.tif") 
plot(myras)

# simple local operation
myras * 2

# How to check if this worked?
myras %>% values() %>% range(na.rm = T)
# What's different here? - all doubled
(myras * 2) %>% values() %>% range(na.rm = T)

## Other simple local operations
myras - 4
myras ** 2
log(myras)

### Reclassify
# Set up reclassification scheme - put into matrix with 3 columns, by row
rcl = matrix(c(0, 1, 0, 2, 249, 1, 250, 256, 0), ncol = 3, byrow = TRUE)
rcl

# Then, apply it - use reclassify() fxn from raster package
validdata = reclassify(myras, rcl = rcl)
validdata
plot(validdata)

# Multiply our "valid" raster with the original
# Sort of like a "mask"
validRaster <- myras * validdata # scalar values * binary
plot(validRaster)

## Can "do algebra" as a function too
# NOAA transform for CHAMPLAIN data
# valid as of 2019-02-01 metadata
# transforms from DN (digital number) to CI
transform_champlain_olci <- function(x){
  10**(((3.0 / 250.0) * x) - 4.2)
}
myras.ci <- validRaster %>% transform_champlain_olci
plot(myras.ci)

## Focal() - start with transformed raster, define our neighborhood (w) as a matrix (here, the dim = 3x3, with each cell having a weight of 1), then 
# For each neighorhood, give me a max
myras_focal = focal(myras.ci, w = matrix(1, nrow = 3, ncol = 3), fun = max)
plot(myras_focal)

# To detect the change, we can plot the difference (subtract one raster from the other)
(myras_focal - myras.ci) %>% plot
# We assumed that the resolution, grid cell points, etc. were all the same

# good practice to verify same extent, projection, resolution, and origin
compareRaster(myras_focal, myras.ci)

# Global operations - on entire dataset (pretty obvious)
# Built in functions
raster::maxValue(myras.ci)
raster::minValue(myras.ci)
# or - do it yourself
myras.ci %>% raster::values() %>% mean(na.rm = T)

# Raster exercise
ras2 <- raster::raster("./data/ts_2016.0902_0908.L4.LCHMP3.CIcyano.MAXIMUM_7day.tif") 
plot(ras2)

