#!/usr/bin/env Rscript
"This is commandline script for H5CellProfiler

Usage: %s [-h] CONFIG_FILE

-h --help    show this
CONFIG_FILE  the YAML config file to use
" -> doc.template

# Function to find the current script
findScriptPath <- function(){
  command.args <- commandArgs(trailingOnly = FALSE)
  file.arg.name <- "--file="
  script.path <- sub(file.arg.name, "", grep(file.arg.name, command.args, value = TRUE))
  return(script.path)
}

# Function to install the pipeline dependencies
installDependencies <- function(){
  installPackages(c('docopt', 'stringr', 'plyr', 'data.table', 'doParallel', 'ggplot2', 'reshape2', 'grid', 'shiny', 'ggvis', 'yaml', 'rmarkdown', 'git2r'))
  installBiocLitePackages(c('rhdf5'))
}

script.path <- findScriptPath()
script.name <- basename(script.path)
script.dir  <- dirname(script.path)

source(file.path(script.dir, "utils", "packages.R"), chdir = TRUE)
installDependencies()

# Now that dependencies are installed we can use docopt
library('docopt')

doc <- sprintf(doc.template, script.name)
my.opts <- docopt(doc)

config.file <- my.opts[['CONFIG_FILE']]
config.path <- normalizePath(config.file)

source(file.path(script.dir, "H5CellProfiler.R"), chdir = TRUE)
runPipeline(config.path)
