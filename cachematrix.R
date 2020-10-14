#Creating a special matrix object that can cache its inverse.
#And, returing the cache of inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  Matrix.M <- NULL
  set <- function(y){
    x <<- y
    Matrix.M <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) Matrix.M <<- inverse
  getInverse <- function() Matrix.M 
  list(set = set, get = get,                    #function to obtain the inverse of the matrix
       setInverse = setInverse, 
       getInverse = getInverse)
}



cacheSolve <- function(x, ...) {          #to get cache data
  
  Matrix.M <- x$getInverse()
  if(!is.null( Matrix.M)){
    message("getting cached data")
    return( Matrix.M)
  }
  mat <- x$get()
  Matrix.M <- solve(mat,...)
  x$setInverse( Matrix.M)
  Matrix.M
}

