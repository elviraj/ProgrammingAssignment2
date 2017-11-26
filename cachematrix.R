## The first function is a matrix constructor function, and the second function calculates
## the inversed matrix and keeps it in cache.

## This function adds to a matrix the methods set/get the matrix, and the setinv/getinv
## to set/get the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setinv <- function(inv) im <<- inv
  getinv <- function() im
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function calculates the inversed matrix for a given matrix. If the inversed matrix 
## is already calculated, it does not calculate it again, since it is already cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  im <- x$getinv()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setinv(im)
  im
}
