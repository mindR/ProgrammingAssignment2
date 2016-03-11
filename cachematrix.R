## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function is a list of functions which can set the 
## matrix value, get the matrix value, 
## set the inversion of the matrix and get the inversion data

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x<<-y
    inv<<-NULL
  }
  get <- function() x
  setinv <- function(inversion) inv <<- inversion
  getinv <- function() inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## cacheSolve function can cache the inversion of matrix value in 
## makeCacheMatrix function. Before caching it checks whether the 
## data is already cached in the makeCacheMatrix function, if yes it 
## retrieves the same data


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
