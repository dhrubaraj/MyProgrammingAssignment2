# makeCacheMatrix creates a list containing a function to
# 1. set the value of a matrix
# 2. get the value of a matrix
# 3. set the value of an inverse matrix
# 4. get the value of an inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv_mtrx <- NULL
  set <- function(y) {
    x <<- y
    inv_mtrx <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv_mtrx <<- inverse
  getinverse <- function() inv_mtrx
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The "cacheSolve" function returns the inverse matrix of a given matrix. 
# The resulted inverse matrix is stored in cache. Next time when the function
# is called again it will return the inverse matrix from cache instead of 
# recalculating it again. This saves considerable processing time. 

# This function assumes that the function argument matrix is always invertible.

cacheSolve <- function(x, ...) {
  inv_mtrx <-x$getinverse()
  if(!is.null(inv_mtrx)) {
    message("getting data from cached.")
    return(inv_mtrx)
  } 	
  data <- x$get()
  inv_mtrx <- solve(data)
  x$setinverse(inv_mtrx)
  inv_mtrx    
}
