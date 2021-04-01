## Input x will be identified as a matrix
## The null will be identified as the solved value "s"
## at the latter part of the section, the word "mean" was modified to "solve"

makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
  s <- NULL
  ser <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
## as said before, "mean" will be changed to "solve" and "m" to "s"
## the cacheSolve will acquire the cache data
cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting inversed matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}