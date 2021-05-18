# greg's harvest1 (base)

# Greg has


# read-in the URL
#harv1tar <- prepInputs(url = "https:/drive.google.com/file/d/1H3eAQjYZLPQzMDIdvlSC0bYfrPm1tp0A",
#                       destinationPath = "inputs")
                       #overwrite = TRUE,
                       #purge = 7)
# this one does not seem to work
# harv1zip <- preProcess(url ="https:/drive.google.com/file/d/1H3eAQjYZLPQzMDIdvlSC0bYfrPm1tp0A",
#                        destinationPath = "inputs")

# problem: the prepTInput call fails...trying to pull it apart.
harv1 <- preProcess(url = "https:/drive.google.com/file/d/1H3eAQjYZLPQzMDIdvlSC0bYfrPm1tp0A",
                    destinationPath = "inputs",
                    targetFile = "tif_scenario-carbon-base_20210422.tar.gz",
                    fun = "utils::untar")
                    #purge = 7)

# find the 5 directories, one per tsa
tsaDirs <- grep("tsa", list.dirs('inputs'))

# this is the table that will get filled with all the firest
fireDistsDT <- data.table(pixelIndex = integer(), year = integer(), events = integer())
cutDistsDT <- data.table(pixelIndex = integer(), year = integer(), events = integer())

# content of one tsa folder
#list.files(list.dirs('inputs')[tsaDirs[1]])

years <- time(sim):end(sim)

# need to the same year in each tsaDirs
# these are the fire and the harvest rasters for each sim year
for(i in 1:length(years)){
  # the files have the same names and are in the same order in the 5 tsa folders
  distTifs <- grep(years[i], list.files(list.dirs('inputs')[tsaDirs[1]]))
  # fire = distTifs[1]
  # cut = distTifs[2]
  fireList <- list()
  cutList <- list()
  for(j in 1:length(tsaDirs)){
    fireList[[j]] <- raster::raster(file.path(list.dirs('inputs')[tsaDirs[j]],
                                   list.files(list.dirs('inputs')[tsaDirs[j]])[distTifs[1]]))
    cutList[[j]] <- raster::raster(file.path(list.dirs('inputs')[tsaDirs[j]],
                                   list.files(list.dirs('inputs')[tsaDirs[j]])[distTifs[2]]))
  }

  # put the 5 rasters together
  fireList$fun <- mean
  fireList$na.rm <- TRUE
  fireRast0 <- do.call(mosaic, fireList)
  fireRast1yr <- postProcess(fireRast0,
                              rasterToMatch = masterRaster)
  fireDT1yr <- data.table(pixelIndex = 1:ncell(fireRast1yr), year = years[i], events = fireRast1yr[])
  fireDistsDT <- rbindlist(list(fireDistsDT,fireDT1yr[!is.na(events)]))

  cutList$fun <- mean
  cutList$na.rm <- TRUE
  cutRast0 <- do.call(mosaic, cutList)
  cutRast1yr <- postProcess(cutRast0,
                             rasterToMatch = masterRaster)
  cutDT1yr <- data.table(pixelIndex = 1:ncell(cutRast1yr), year = years[i], events = cutRast1yr[])
  # cut events = 2
  cutDT1yr[!is.na(events)]$events <- 2
  cutDistsDT <- rbindlist(list(cutDistsDT,cutDT1yr[!is.na(events)]))

}



# THIS WORKS FOR ONE YEAR
#
# distTifs <- grep(time(sim), list.files(list.dirs('inputs')[tsaDirs[1]]))
# list.files(list.dirs('inputs')[tsaDirs[1]])[distTifs]
#
# fireRasttsa08 <- raster::raster(file.path(list.dirs('inputs')[tsaDirs[1]],
#                                          list.files(list.dirs('inputs')[tsaDirs[1]])[distTifs[1]]))
# fireRasttsa16 <- raster::raster(file.path(list.dirs('inputs')[tsaDirs[2]],
#                                           list.files(list.dirs('inputs')[tsaDirs[2]])[distTifs[1]]))
# fireRasttsa24 <- raster::raster(file.path(list.dirs('inputs')[tsaDirs[3]],
#                                           list.files(list.dirs('inputs')[tsaDirs[3]])[distTifs[1]]))
# fireRasttsa40 <- raster::raster(file.path(list.dirs('inputs')[tsaDirs[4]],
#                                           list.files(list.dirs('inputs')[tsaDirs[4]])[distTifs[1]]))
# fireRasttsa41 <- raster::raster(file.path(list.dirs('inputs')[tsaDirs[5]],
#                                           list.files(list.dirs('inputs')[tsaDirs[5]])[distTifs[1]]))
# fire2020 <- list()
# fire2020[[1]] <- fireRasttsa08
# fire2020[[2]] <- fireRasttsa16
# fire2020[[3]] <- fireRasttsa24
# fire2020[[4]] <- fireRasttsa40
# fire2020[[5]] <- fireRasttsa41
#
# fire2020$fun <- mean
# fire2020$na.rm <- TRUE
# fire2020rast1 <- do.call(mosaic, fire2020)
#
#
# fireRast2020 <- postProcess(fire2020rast1,
#                             rasterToMatch = masterRaster)
# fireThisYr <- data.table(pixelIndex = 1:ncell(fireRast2020), year = time(sim), events = fireRast2020[])
#
#
# # make one big DT for the fires
# fireDists <- rbindlist(list(fireDists,fireThisYr[!is.na(events)]))
#



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
