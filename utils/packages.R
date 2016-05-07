arePackagesInstalled <- function(packages) {
  return(sapply(packages, require, character.only = TRUE))
}

installPackages <- function(packages) {
  installed <- arePackagesInstalled(packages)
  lapply(packages[!installed], install.packages, repos="http://cran.rstudio.com/")
}

installBiocLitePackages <- function(packages) {
  installed <- arePackagesInstalled(packages)
  if(!all(installed)) {
    source("http://bioconductor.org/biocLite.R")
    biocLite(packages[!installed], suppressUpdates = TRUE)
  }
}
