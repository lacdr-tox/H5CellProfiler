config.file <- '~/src/H5CellProfiler/example_config.yaml'

prependInputDirectoryToPath <- function (config) {
  config$modules$`extract-hdf5`$input$hdf5 <-
    file.path(config$io$`input-directory`,
              config$modules$`extract-hdf5`$input$hdf5)
  config$modules$`extract-hdf5`$input$layout <-
    file.path(config$io$`input-directory`,
              config$modules$`extract-hdf5`$input$layout)
  return(config)
}

main <- function() {
  old.wd <- getwd()
  config <- yaml::yaml.load_file(config.file)
  config <- prependInputDirectoryToPath(config)
  out.dir <- config$io$`output-directory`
  if(!dir.exists(out.dir)) {dir.create(out.dir)}
  setwd(out.dir)
  source(file.path(config$`pipeline-location`, "H5CellProfiler.R"), chdir = TRUE)
  invisible(lapply(config$run), function (module) {
      print(paste("Running module", module))
      module.function <- getFunctionForModule(module)
      module.function(config$modules[[module]], config$cores)
  })
  setwd(old.wd)
  print("done")
}
