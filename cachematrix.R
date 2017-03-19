## These functions are designed to store matrix inversions to cache.
## The entire makeCacheMatrix() environment stays in memory, and cacheSolve 
## can access its functions as well as any data in that environment 
## that is referenced in its functions.

## This function passes the inverse (solve()) of a matrix into cache, 
## clearing any matrix that may have already been in cache.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function is required to populate or retrieve the matrix 
## inverse from makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if(!is.null(s)) {
      message("getting cached data")
      return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
  }
