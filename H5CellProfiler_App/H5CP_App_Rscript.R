#!/usr/bin/env Rscript
"This is commandline script for H5CellProfiler App

Usage: %s [-h]

-h --help    show this
" -> doc.template


# check if pacman (package manager) is installed
if (!require("pacman")) install.packages("pacman"); library(pacman)
# install the pipeline dependencies
p_install_version('docopt', '0.4.5')
p_load('docopt')

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

source(file.path(script.dir, "H5CP_App.R"), chdir = TRUE)
app <- getApp(input.dir = NULL)
app
