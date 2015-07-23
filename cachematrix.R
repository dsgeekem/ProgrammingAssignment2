##This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setmatrix <- function(z) m <<- z
  getinv <- function() m
  list(set = set, get = get,
      setmatrix = setmatrix,
      getinv = getinv)

}


##This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix 

##If the inverse has already been calculated (and the matrix has not changed), 
##then the this function will retrieve the matrix from cache/memory instead of
##recalculating the inverse matrix again.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## Return a matrix that is the inverse of 'x'
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}