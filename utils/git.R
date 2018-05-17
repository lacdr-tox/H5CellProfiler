library(git2r)

getGitInformation <- function(directory) {
  is.repo <- T
  tryCatch({
    repo <- repository(directory)
  }, error = function(e) {
    #stop(paste(normalizePath(directory), "is not a git repository"))
    is.repo <<- F
  })
  if(!is.repo) {
    return(list('summary' = "Not a git repository", 'branch' = '', 'diff' = ''))
  }
  summary <- capture.output(show(repo))
  branch <- head(repo)@name
  diff <- diff(repo, as_char = TRUE)
  return(list('summary' = summary, 'branch' = branch, 'diff' = diff))
}