## There are two funciton in this file
## First function creats a matrix object that can cache its inverse
## The second function computes the inverse of matrix
##

## Creats list of function to get and set (inverse of) matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMatInverse <- function(solve) m <<- solve
  getMatInverse <- function() m
  list(set=set, get=get, setInv=setInv,getInv=getInv)
  
}


## If the inverse of matrix is available, it is assigned, otherwise computed
## and committed

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getMatInverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}
