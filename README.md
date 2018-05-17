# H5CellProfiler
R-package. Data analysis and graphics package with a browser based GUI. Functions as interface for the HDF5-format output from the open source image-analysis tool CellProfiler

Note: not installable/ packagable yet

## H5CellProfiler bat versions (Windows only)

You need to add pandoc to your `PATH`. First start RStudio and execute 

```r
Sys.getenv('RSTUDIO_PANDOC')
```

You should follow the instructions [here](https://superuser.com/a/732634/143446) to add an environment variable.
The variable should be `PATH` and the value should be what you get from the above command, but without `pandoc.exe` (most likely it is something like `C:/Program Files/RStudio/bin/pandoc`)
