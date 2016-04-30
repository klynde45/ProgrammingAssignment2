## These two functions cache the time-consuming computation of 
## matrix computations, saving time when it deals with longer matrices.

## This function sets the value of the vector, its inverse, and caches it

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setmatrix <- function(inverse) inv <<- inverse
  getmatrix <- function() inv
  list(set=set, get=get, setinverse=setinverse, 
       getinverse=getinverse)
}


## This function returns the inverse of the matrix.

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}

