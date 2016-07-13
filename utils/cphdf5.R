# Look at https://github.com/CellProfiler/CellProfiler/wiki/Module-structure-and-data-storage-retrieval#HDF5
# for CellProfiler HDF5 info

library(rhdf5)

cphdf5_fixed_measurement_groups <- c("Experiment", "Image", "Relationship")

getCPH5Dump <- function(h5file) {
  h5dump(h5file, load = FALSE)
}

getMeasurementsFromDump <- function(h5dump) {
  # First nesting in Measurements group is the timestamp, so we skip that one
  h5dump$Measurements[[1]]
}

getMeasurementsFromCPH5 <- function(h5file) {
  getMeasurementsFromDump(getCPH5Dump(h5file))
}

getMeasurementsGroups <- function(measurements) {
  names(measurements)
}

getObjectsFromMeasurements <- function(measurements) {
  setdiff(getMeasurementsGroups(measurements), cphdf5_fixed_measurement_groups)
}

getMetadataFromMeasurements <- function(measurements, prefix = TRUE) {
  metadata <- grep("^Metadata_", names(measurements$Image), value = TRUE)
  if(!prefix) {
    return(metadata)
  }
  paste("Image/", metadata, sep = "")
}

getFeaturesFromMeasurements <- function(measurements, objects) {
  return(unlist(lapply(objects, function (object) {
    features <- names(measurements[[object]])
    paste(object, "/", features, sep = "")
  })))
}
