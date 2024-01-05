makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setrev <- function(rev) m <<- rev
  getrev <- function() m
  list(set = set, get = get,
       setrev = setrev,
       getrev = getrev)
}
cacheSolve <- function(x, ...) {
  m <- x$getrev()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- rev(x)
  x$setrev(m)
  m
}

