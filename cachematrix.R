## Caching the Inverse of a Matrix
## The assignment is to write a pair of functions that cache the inverse of a matrix
## it may make sense to cache the value of the inverse of a matrix so that when we need it again
## if the contents of the matrix are not changing,
## it can be looked up in the cache rather than recomputed

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## The first function, makeCacheMatrix creates a special "matrix",
## which is really a list containing a function
## 1- set(): set the value of the matrix
## 2- get(): get the value of the matrix
## 3- setinverse(): set the value of the inverse of the matrix
## 4- getinverse(): get the value of the inverse of the matrix

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

## cacheSolve: This function calculates the inverse of the special "matrix" created with the makeCacheMatrix function.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheinverse <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}