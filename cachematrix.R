## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a special "matrix" object that caches it's 
##   inverse(well whatever is 'set' actually I guess - just calling it inverse here as that is what we will set in the next function).
##  The double << sets the value in the parent environemnt
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(inverse) m <<- inverse
  getmatrix <- function() m
  list (set= set, get= get, setmatrix = setmatrix, getmatrix = getmatrix )
}


## Write a short comment describing this function
##This function checks whether there is a cached matrix already, and if there is then it returns that 
## otherwise it 'solves' the matrix and cahces that by using the setmatrix function of the makeCacheMatrix function


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m  
}
