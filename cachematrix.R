## Since calculating the inverse of a Matrix using Solve() is an expensive computation, it is faster
## to spped up this computation by using a cache to store computed values and 
## retrieve the matrix inverse from this cache if available

## The first function, makeCacheMatrix creates a special "vector", which is really
## a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function () x
  setmatrixinverse <- function(matrixinverse) m <<- matrixinverse
  getmatrixinverse <- function() m
  list (set = set, get = get, 
        setmatrixinverse = setmatrixinverse, 
        getmatrixinverse = getmatrixinverse)
}


## The following function calculates the inverse of the special "vector" 
## created with the above function. However, it first checks to see if the 
## inverse of the matrix has already been calculated. If so, it gets the matrix 
## from the cache and skips the computation. Otherwise, it calculates the inverse
## of the matrix and sets the value of the matrix in the cache via the setmatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrixinverse()
  if (!is.null(m)) {
    message("getting inverse from cache")
    return(m)
  }
  data <- x$get()
  message("getting inverse")
  m <- solve(data, ...)
  x$setmatrixinverse(m)
  m
}
