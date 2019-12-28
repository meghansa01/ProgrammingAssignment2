#Attempting to make a cahced martix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    m <<- matrix()
  }
  get <- function() x
  #giving ability to set and retrieve inverse of matrix in funtion
  setinverse <- function(inv) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#Attempting the function for computing the inverse of the matrix via the "solve" function
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
