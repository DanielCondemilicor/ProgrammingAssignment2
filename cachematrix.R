## makeCacheMatrix stores data in the variable inv and x. then cacheSolve is the one performing the solve matrix inverse function
## this function solves for the inverse of 2*2 matrices.

## makeCacheMatrix is a function that caches the matrix with the use of closure functions (<<-)
## just like the cache vector example. 

##this function creates an environment and calls it for the other function :cacheSolve() to use.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  
  set <-function(a){ x <<- a
  inv<<- NULL
  }
  get <- function() x #first class function that calls the given matrix
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv #first class function that calls inv
  
  ##
  list(set=set, setinv=setinv, getinv = getinv, get = get)
  
}


## this function grabs the first class functions in the "cached matrix" data 
## and solves its inverse

cacheSolve <- function(a, ...) {
  matrix <- a$get()
  inverse <- a$getinv()
  inverse <- solve(matrix, ...)
  a$setinv(inverse)
  inverse
}
