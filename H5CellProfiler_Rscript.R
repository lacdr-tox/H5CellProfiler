#!/usr/bin/Rscript
"This is commandline script for H5CellProfiler

Usage: %s [-h] CONFIG_FILE

-h --help    show this
CONFIG_FILE  the YAML config file to use
" -> doc.template

library('docopt')

# Function to find the current script
findScriptPath <- function(){
  command.args <- commandArgs(trailingOnly = FALSE)
  file.arg.name <- "--file="
  script.path <- sub(file.arg.name, "", grep(file.arg.name, command.args, value = TRUE))
  return(script.path)
}

script.path <- findScriptPath()
script.name <- basename(script.path)
script.dir  <- dirname(script.path)

doc <- sprintf(doc.template, script.name)
my.opts <- docopt(doc)

config.file <- my.opts[['CONFIG_FILE']]
config.path <- normalizePath(config.file)

source(file.path(script.dir, "H5CellProfiler.R"), chdir = TRUE)
runPipeline(config.path)