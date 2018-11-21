## These two functions, written as part of the Data Science course on Coursera,  
## are designed to work with matrixes that can cache their inverse. 

## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {
    x
  }
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  getInverse <- function() {
    inv
  }
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), then the 
##  cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

## Testing the function

TestM <- makeCacheMatrix(matrix(1:4,2,2))
cacheSolve(TestM)
cacheSolve(TestM)

> TestM <- makeCacheMatrix(matrix(1:4,2,2))

> cacheSolve(TestM)
      [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5

> cacheSolve(TestM)
getting cached data
      [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5