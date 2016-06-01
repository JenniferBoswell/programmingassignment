## Matrix inversion is usually a costly computation and there
## may be some benefit to caching the inverse rather than
## computing it repeatedly.  
## The following functions 
## 1. create a special "matrix" object that can cache its inverse
## 2. computes the inverse of the special "matrix". 


## This function creates the special "matrix" object that can cache its inverse. 

##makeCacheMatrix 
## sets the value of the matrix
## gets the value of the matrix
## sets the value of the inverse matrix
## gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function creates a special matrix object that can cache
## its inverse. 

cachesolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
        ## Cachesolve returns a matrix that is the inverse of 'x'
