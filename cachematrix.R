## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: usage mx <- makeCacheMatrix(m) 
## where (m) is a matrix
## A list (mx) is created containing the following functions:
##      mx$getmat(): gets the matrix (m)
##      mx$setmat(new_m): sets the matrix (new_m)
##      mx$getinvmat(): retrieves the inverse of the matrix stored in cache
##      mx$setinvmat(invmat): stores the inverse of the matrix to cache

makeCacheMatrix <- function(x = matrix()) {
  
    ## Initialize variables     
    invmat <- NULL
    
    
    setmat <- function(y) { ## sets the matrix
      x <<- y
      invmat <<- NULL
    } 

    getmat <- function() x  ## gets the matrix
    
    getinvmat <- function() invmat  ## retrieves the inverse of the matrix
    setinvmat <- function(cache) invmat <<- cache ##stores the inverse to cache
    
    list(setmat = setmat, getmat = getmat, 
         setinvmat = setinvmat,
         getinvmat = getinvmat)  ## returns list
}



## CacheSolve: usage CacheSolve(mx)
## This function computes the inverse of the special "matrix" (x)
## returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
  
    ## retrieve the inverse from the cache
    invmat <- x$getinvmat()
    
    
    ## If the inverse has already been calculated, 
    ## then return the results from cache.
    
    if(!is.null(invmat)) {
      message("getting cached data")
      return(invmat)
    }
    
    ## else compute the inverse of the matrix and set result to cache    
    data <- x$getmat()
    invmat <- solve(data, ...)
    x$setinvmat(invmat)
    invmat    
}
