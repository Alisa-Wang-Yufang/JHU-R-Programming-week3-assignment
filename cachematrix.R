## This function creates a special "matrix" object that can cache its inverse.
## x is the matrix object that user will submit on the console

##Function makeCacheMatrix creat a special matrix
##and return a list containing function set, get, setInverse, and getInverse.

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv_x
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.
##If list returned by rhe first function is null in getInverse, This function will 
##calculated by itself.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'sample'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv_x)
  }
  mat <- x$get()
  inv_x <- solve(mat, ...)
  x$setInverse(inv_x)
  inv_x
}
