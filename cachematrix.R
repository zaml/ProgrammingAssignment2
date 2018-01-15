## --------------------------------------
## -- Juan Zamora
## -- Programming Assignment 2: Lexical Scoping
## -- submitted: 14/1/2018
## --------------------------------------

## Function that creates the special matrix that 
## is able to cache the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  
  # inverse matrix 
  matinv <- NULL
  
  # initialize object
  setMatrix <- function(y) {
    x <<- y
    matinv <<- NULL
  }
  
  # obtain matrix
  getMatrix <- function() x
  
  # get current inverse value
  getInverse <- function() matinv
  
  # set inverse value
  setInverse <- function(inv) matinv <<- inv 
  
  # list functions
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       getInverse = getInverse,
       setInverse = setInverse)
}


## Function that returns inverse from cache
## if no cache available, it will calculate
## the matrix inverse using Solve. Solve requires
## the matrix to be squared, so there is a validation
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # obtain inverse cached matrix 
  i <- x$getInverse()
  
  # if there is data in cache, then return it
  if(!is.null(i)) {
    message("getting cached inverse matrix data")
    return(i)
  }
  
  ## -- in case there is no cached matrix -- 
  
  # obtains matrix
  m <- x$getMatrix()
  
  # is cols and rows are not the same, solve will fail.
  if(nrow(m) != ncol(m))
  {
    message("matrix dimension mismatch")
    return (NULL)
  }
  
  print(m)
  
  # obtain inverse from m
  i <- solve(m)
  
  # add inverse to cache obj
  x$setInverse(i)
  
  # return inverse
  return (i)
  
}



# Test Area --------------------------------------
set.seed(1024)

# matrix object creation
mat <- matrix(rnorm(9), nrow=3, ncol=3)
o <- makeCacheMatrix()
o$setMatrix(mat)

# run first time to generate cache
cacheSolve(o)

# run second time to READ cache
cacheSolve(o)
# Test Area --------------------------------------




