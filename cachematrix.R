## The followinfg two functions are used to demonstrate a programming 
## method to cache results of some expensive calculations.
## The <<- operator is also introducted to assign a value to an objecct that
## is in different environment.

## Thw makeCascheMatrix function creates a customized matrix object that can cache the inverse of the matrix.
## The function is a list of functions to
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse of the matrix
## 4. set the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y     ## Set the new matrix value
        i <<- NULL  ## Set the cache to be NULL since a new matrix value is set 
    }
    
    ## return the value of the current matrix
    get <- function() x 
    
    ## put the inverse of the matrix in cache
    setinv <- function(inv) i <<- inv 
    
    ## get the inverse value from cache
    getinv <- function() i    
    
    ## return the list of functions 
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The function computes the inverse of the special "matrix" returned by
## makeCacheMatrix. In the case that the inverse of the matrix is already
## calculated and cached, the function should retrive the inverse from the 
## cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    
    ## if the cache returns a value
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## If there is no cache value for the current matrix
    ## Retrieve the value of th ematrix
    data <- x$get()
    ## calculate the inverse of the matrix
    i <- solve(data, ...)
    ## Put the inverse value back to the special matrix object
    x$setinv(i)
    ## Return the newly calculated inverse value of the matrix
    i
}
