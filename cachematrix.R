## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## 1. makeCacheMatrix: This function creates a special "matrix" object
##that can cache its inverse.

##2. cacheSolve: This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already been 
##calculated (and the matrix has not changed), then the cachesolve should 
##retrieve the inverse from the cache.

##04-03-2018

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  
  get <- function() x
  setinv <- function(solve) inv_matrix <<- solve
  getinv <- function() inv_matrix
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Write a short comment describing this function
##This function generates the inverse of the matrix 
## created by makeCacheMatrix above.

## it first checks to see if the inverse of matrix has already been calculated. 
## If so, it gets the Inv_matrix from the cache and skips the computation

##Otherwise, it calculates the inverse of mean of the data and sets the value of the mean
## in the cache via the setinv function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv_matrix <- x$getinv()
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  data <- x$get()
  inv_matrix <- solve(data, ...)
  x$setinv(inv_matrix)
  inv_matrix
  
  
}
