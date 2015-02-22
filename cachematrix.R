## Coursera rprog-011 assignment 2

## makeCacheMatrix - create a special object that stores a matrix and caches its inverse
##              clears the cache if we pass a new value
## cacheSolve - return the inverse of a matrix, checks to see if its cached and returns the 
##              cached value rather than recalculating
##

## makeCacheMatrix creates an object containing a special matrix which can 
## be accessed via functions in a list 
##
## we call the function with an argument of a matrix, and from the returned
## object we can
##
## a) set the value of our matrix
## b) get the value of our matrix
## c) set the value of the inverse 
## d) get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function (y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns an inverse matrix from an object created by makeCacheMatrix
## 
## internally it checks the object to see if a cache exists, if it finds one it 
## returns that value, otherwise it calls solve() on the value of the matrix we get
## from the makeCacheMatrix object, calls the setinverse() function to cache the 
## updated value, and then returns this value


cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  
  if ( !is.null(i)) {
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
