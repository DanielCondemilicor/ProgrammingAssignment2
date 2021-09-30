## makeCacheMatrix stores data in the variable inv and x. then cacheSolve is the one performing the solve matrix inverse function
## this function solves for the inverse of 2*2 matrices.

## makeCacheMatrix is a function that caches the matrix with the use of closure functions (<<-)
## just like the cache vector example. 
##this sets teh value of matrix
## get value of matrix
## set the value of inverse
## and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ##inv is a emptyy vector
  
  ##collects data from matrix???
  set <-function(y){ 
    x <<- y 
    inv<<- NULL
  }
  
  ##
  get <- function() x 
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  ##
  
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## this function grabs the first class functions in the "cached matrix" data 
## and solves its inverse

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)){ 
        message("grabbing that very delicious cached data :)")
        return(inv)
  }
  
  ##
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinv(inv)
  inv
  ##
        ## Return a matrix that is the inverse of 'x'
}
