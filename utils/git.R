library(git2r)

getGitInformation <- function(directory) {
  tryCatch({
    repo <- repository(directory)
  }, error = function(e) {
    stop(paste(normalizePath(directory), "is not a git repository"))
  })
  summary <- capture.output(show(repo))
  branch <- head(repo)@name
  diff <- diff(repo, as_char = TRUE)
  return(list('summary' = summary, 'branch' = branch, 'diff' = diff))
}