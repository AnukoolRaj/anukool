## makeCacheMatrix function makes a mtrix which can save
##its inverse in for quick reffrence later on
## cacheSolve function solves the inverse of matrix
##if it has been not already solve else fecthes it from cache

## this function makes a matrix cachable


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}
    ## this function retrives the inverse of the matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {     ## checking if the inverse is available or not, if no, it has to be computed via solve()
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
