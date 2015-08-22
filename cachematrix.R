## makeCacheMatrix creates and returns a list of get/set functions used 
## to calculate the inverse of a matrix and cache the result, so that 
## it returns the cached value in case it had already calculated it 
## previously on the same matrix.
makeCacheMatrix <- function(x = matrix()) {
  # m is the cached result. It is initialized to NULL as there are
  # no calculations cached yet.
  m <- NULL
  
  # set is used to assign a matrix. As we have new data, we have to
  # initialize the cache as it is not valid anymore.
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # get returns the matrix.
  get <- function() x
  
  # set inverse stores the inverted matrix in the cache
  setinverse <- function(inverse) m <<- inverse
  
  # getinverse returns the cached inverted matrix
  getinverse <- function() m
  
  # returns the functions created to set and get the matrix and to
  # set and get the inverted matrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve is used for the actual calculation of the inverse of 
## the matrix stored inside makeCacheMatrix.
## It first checks if there is a cached value and returns it if it
## exists, otherwise it calculates the inverse of the matrix and stores
## the value in the cache so next time there is no need to make the
## calculations again.
cacheSolve <- function(x, ...) {
  # Get the cached value of the inverse
  m <- x$getinverse()
  # If there is a cached value, it is returned instead of making 
  # the calculation
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # There was no cached data, so we get the matrix into "data"
  data <- x$get()
  # We calculate the inverse of the matrix
  m <- solve(data, ...)
  # We store the inverted matrix on the cache
  x$setinverse(m)
  # Return the inverted matrix
  return(m)
}
