# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. Below are
# two functions used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing functions to:
#   set - set the value of the matrix
#   get-  get the value of the matrix
#   setInv- set the value of inverse of the matrix
#   getInv- get the value of inverse of the matrix

makeCacheMatrix <- function (x = matrix()){
  
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setInv <- function(inverse) i <<- inverse
  getInv <- function() i
  
  list(set = set, get = get, setInv = setInv, getInv = getInv)    
}


# cacheSolve returns the inverse of the matrix returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then 
# the function will retrieve the inverse from the cache.  If not, the inverse will be
# calculated and cached for future refernce 

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...){
  
  i <- x$getInv()
  
  if(!is.null(i)) {
    message("getting cached matrix inverse")
    return(i) }
  
  data <- x$get()
  
  i <- solve(data, ...)
  
  x$setInv(i)
  
  i
}
