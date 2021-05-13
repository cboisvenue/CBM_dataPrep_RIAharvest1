# greg's harvest1 (base)

# Greg has

# read-in the URL
harv1tar <- prepInputs(url = "https:/drive.google.com/file/d/1H3eAQjYZLPQzMDIdvlSC0bYfrPm1tp0A",
                       destinationPath = "inputs")
# this one does not seem to work
# harv1zip <- preProcess(url ="https:/drive.google.com/file/d/1H3eAQjYZLPQzMDIdvlSC0bYfrPm1tp0A",
#                        destinationPath = "inputs")

# find the 5 directories, one per tsa
tsaDirs <- grep("tsa", list.dirs('inputs'))

# try one year
list.files(list.dirs('inputs')[tsaDirs[1]])

time(sim):end(sim)

# need to the same year in each tsaDirs
# these are the fire and the harvest rasters for this sim year
distTifs <- grep(time(sim), list.files(list.dirs('inputs')[tsaDirs[1]]))
list.files(list.dirs('inputs')[tsaDirs[1]])[distTifs]

fireRasttsa08 <- raster::raster(file.path(list.dirs('inputs')[tsaDirs[1]],
                                         list.files(list.dirs('inputs')[tsaDirs[1]])[distTifs[1]]))
fireRasttsa16 <- raster::raster(file.path(list.dirs('inputs')[tsaDirs[2]],
                                          list.files(list.dirs('inputs')[tsaDirs[2]])[distTifs[1]]))
fireRasttsa24 <- raster::raster(file.path(list.dirs('inputs')[tsaDirs[3]],
                                          list.files(list.dirs('inputs')[tsaDirs[3]])[distTifs[1]]))
fireRasttsa40 <- raster::raster(file.path(list.dirs('inputs')[tsaDirs[4]],
                                          list.files(list.dirs('inputs')[tsaDirs[4]])[distTifs[1]]))
fireRasttsa41 <- raster::raster(file.path(list.dirs('inputs')[tsaDirs[5]],
                                          list.files(list.dirs('inputs')[tsaDirs[5]])[distTifs[1]]))
fire2020 <- list()
fire2020[[1]] <- fireRasttsa08
fire2020[[2]] <- fireRasttsa16
fire2020[[3]] <- fireRasttsa24
fire2020[[4]] <- fireRasttsa40
fire2020[[5]] <- fireRasttsa41
#trying mosaic
fire2020$fun <- mean
fire2020$na.rm <- TRUE
fire2020rast1 <- do.call(mosaic, fire2020)
# merge seems to put in on disk instead of in RAM,
# that is the only difference I could see
# fire2020 <- list()
# fire2020[[1]] <- fireRasttsa08
# fire2020[[2]] <- fireRasttsa16
# fire2020[[3]] <- fireRasttsa24
# fire2020[[4]] <- fireRasttsa40
# fire2020[[5]] <- fireRasttsa41
# #trying merge
# fire2020$filename <- 'try2'
# fire2020$overwrite <- TRUE
# fire2020rast2 <- do.call(raster::merge, fire2020)

fireRast2020 <- postProcess(fire2020rast1,
                            rasterToMatch = masterRaster)




# Eliot's
fun <- function(targetFilePath) {
  out <- untar(targetFilePath)
  something else to read it into R depending on what type of object it is
}
prepInputs(..., fun = quote(fun(targetFilePath)))

# this is the url
#https:/drive.google.com/file/d/1H3eAQjYZLPQzMDIdvlSC0bYfrPm1tp0A


# #Greg's
# loadAges <- function(sim) {
#   year <- as.integer(time(sim) - start(sim) + P(sim)$base.year)
#   files <- sapply(P(sim)$basenames,
#                   function(bn) file.path(inputPath(sim),
#                                          P(sim)$tifPath,
#                                          bn,
#                                          paste("inventory_", toString(year), ".tif", sep="")))
#   x <- sapply(files, raster, band=2)
#   if (length(x) > 1) {
#     names(x)[1:2] <- c("x", "y") #from the raster pkg mosaic help. Needs x and y (!?)
#     x$fun <- mean
#     x$na.rm <- TRUE
#     r <- do.call(mosaic, x)
#     r[is.nan(r)] <- NA # replace NaN values with NA
#   } else {
#     r <- x[[1]]
#   }
#   names(x) <- NULL
#   return(r)
# }
