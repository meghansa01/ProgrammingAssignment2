makeCacheMatrix <- function(x = matrix()) {
  matrix <- matrix()
  set <- function(y) {
    x <<- y
    m <<- matrix()
  }
  get <- function() x
  setinverse <- function(matrix) m <<- matrix
  getinverse <- function(solve) m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

cachesolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}