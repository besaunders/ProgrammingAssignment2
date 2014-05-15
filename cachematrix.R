## cachematrix.R

## This program contains two routines that allow caching of the
## the inverse of a matrix. One routine creates list of functions
## that can be performed on the matrix object, and the other
## routine retrieves the cached inverse, or calculates the inverse
## if no cached value is present and then sets the cache value.


## This defines the operations allowed on the matrix object: to set
## it or retrieve it, and to set its inverse or retrieve it. If the
## value gets set to a new value, the cache is cleared.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setminv <- function(solve) m <<- solve
  getminv <- function() m
  list(set = set, get = get,
       setminv = setminv,
       getminv = getminv)
}


## This function retrieves the cached value of the matrix inverse,
## and recalculates if it does not exist. The inverse is
## calculated with the solve function.

cacheSolve <- function(x, ...) {
  m <- x$getminv()
  if(!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setminv(m)
  m
}

## This commands illustrate creating the matrix object, setting it
## (with random values), and then calling the matrix inversion
## routine twice (to demonstrate the time savings from caching)

mat4M <- makeCacheMatrix()
mat4M$set(matrix(rnorm(4000000,10,2),2000,2000))
system.time(invmat4M <- cacheSolve(mat4M))
system.time(invmat4M <- cacheSolve(mat4M))

