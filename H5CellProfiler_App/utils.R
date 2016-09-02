# Inverts a named vector, key become values, values become keys
invertVector <- function(x) {
  # Implementation from searchable package
  if( is.null( names(x) ) ) stop( "vector does not have names.")
  v <- names(x)
  names(v) <- as.character(x)
  return(v)
}

# Inspired by Javas Optional orElse construct,
# tries to read value, if value is not NULL returns it's value, otherwise return the else_value
getOrElse <- function(x, else_value) {if(!is.null(x)) x else else_value}
