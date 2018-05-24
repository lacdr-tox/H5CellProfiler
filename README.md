# H5CellProfiler
R-package. Data analysis and graphics package with a browser based GUI. Functions as interface for the HDF5-format output from the open source image-analysis tool CellProfiler

Note: not installable/ packagable yet

## H5CellProfiler bat versions (Windows only)

### Adding pandoc to PATH

You need to add pandoc to your `PATH`. First start RStudio and execute 

```r
Sys.getenv('RSTUDIO_PANDOC')
```

You should follow the instructions [here](https://superuser.com/a/989665/143446) to add an environment variable.
The variable should be `PATH` (or `Path` )
 * If that variable already exists, add a `;` (semicolon) at the end plus the directory you get from the above command, but without `pandoc.exe` (most likely `C:/Program Files/RStudio/bin/pandoc`). (In Windows 10 you can also double click the `Path` entry and then just past the directory in a new row)
 * If it doesn't exist you should create it

### Adding powershell to PATH (only if it complains about not being able to find it)

Most likely powershell is at `C:\Windows\System32\WindowsPowerShell\v1.0` so go there to check if that is indeed the case, and append it to your Path environment variable using the instructions above
