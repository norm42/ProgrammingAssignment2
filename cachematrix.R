## makecacheMatrix takes a matrix and generates a list of functions
## These functions can be used to set or get the matrix (set, get),
## set or get the inverse (setinv, getinv).
##
## See cacheSolve for usage
##
## Norm Zeck; For coursera R programming peer assignement
## 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function will generate the inverse of a matrix set with 
## makeCacheMatrix.  if the inverse has been generated before 
## and is in the environment, the cached inverse will be used
##
## Norm Zeck; For coursera R programming peer assignement
##
## typical usage
## a <- makeCacheMatrix(matrix(1:4,2))  ## Set size of matrix
## a$set(matrix(5:8,2)  ## set matrix values
## a$get()  ## this would return the matrix 
## ainv <- cacheSolve(a)  ## returns the inverse of a
## a$getinv()  ## returns inverse
## a$setinv(matrix(5:8,2))  ## will set the inverse to the matrix passed
##
## if a cached value is found in the environment, the message is printed
## "getting cached data" indicating the inverse was found in the cache
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
