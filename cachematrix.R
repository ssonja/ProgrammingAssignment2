## 'makeCacheMatrix' creates a special object which wraps a matrix and 
## contains a list of functions to get/set the matrix and its inverse:
## set() sets the matrix
## get() returns the matrix
## setinv() sets the inverse matrix
## getinv() returns the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(newinv) inv <<- newinv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  

}

## 'cacheSolve()' returns the inverse of a matrix 'x' where 'x' is a 
## special matrix created with makeCacheMatrix above. If it is the 
## first time cacheSolve() is called for this matrix, the inverse 
## will be calculated using solve() function and the result remembered (cached). 
## Any subsequent calls to 'cacheSolve()' will return the cached value.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  y <- x$get()
  inv <- solve(y)
  x$setinv(inv)
  inv
  
}
