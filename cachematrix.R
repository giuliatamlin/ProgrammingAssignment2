# this function creates a special "matrix" object which stores in the
# cache a matrix and its inverse, by taking as input the matrix to be inverted.
# The function returns a special object consisting of a list of 
# four functions: the get functions read the value of the matrix and
# of its inverse, the set functions store their values to cache.
# The result of running this function alone is to read the input matrix
makeCacheMatrix <- function(x= matrix()) {
  inv <- NULL # sets the value of the inverse to default i.e. the null object
  setmatrix <- function(y) {
  
    x <<-y # stores the value of the input matrix in the cache i.e. in x
    inv <<- NULL #sets the value of the inverse to default (it is necessary 
    # as whenever the function set matrix is used, the inverse needs to be 
    # recomputed and so any cached value needs to be re-initialized)
  }
  getmatrix <- function() # gets the value of the input matrix
  {x}
  setinverse <- function(inverse) # cache the value of the inverse
  {inv<<-inverse}
  getinverse <- function() inv # get the  value of the inverse
  list(set = setmatrix, get = getmatrix,  
       setinverse = setinverse,
       getinverse = getinverse)
  # list the four functions that are used to cache and get the value of the 
  # matrix and its inverse. Are those attributes of the "special" matrix object?
}


## this function checks whether the matrix and its inverse have already been 
# stored in the cache and, if not, recomputes their values. It takes
# as input the special matrix object calculated by the previous function

cacheSolve <- function(x, ...) {
  
  # first step is to retrieve the cached value of the inverse
  inv <- x$getinverse()
  # if the cached value of the inverse is non null and if the cached and the 
  # input matrix are the same (which guarantees that the cached inverse belongs 
  # to the correct matrix), then return the cached inverse
  if (!is.null(inv) & x$set() == x$get()){
      message("getting cached data")
      return(inv)
  }
  # otherwise compute the inverse
  # step 1: read the matrix whose inverse is to be computed
  mat <- x$get() 
  x$set(mat) # step 2: cache the input matrix
  inv <- solve(mat,...) # step 3: compute its inverse
  x$setinverse(inv) # step 4: cache it
  return(inv) # step 5: return it
}
