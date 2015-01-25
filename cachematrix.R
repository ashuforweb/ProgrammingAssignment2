# cachematrix.R contains two function makeCacheMatrix and cacheSolve, makeCacheMatrix accepts an
# invertible matrix this function has list of functions for setting, getting matrix and 
# setting and getting inverse of an invertible matrix
# Following are the functions and what the do
# 1. set - set the value of an invertible matrix
# 2. get - get the value of an invertible matrix set by makeCacheMatrix
# 3. setSolve - set the inverse of an invertible matrix
# 4. getSolve - get the inverse of an invertible matrix
# cacheSolve function get the inverse of an invertible matrix and cache it for future use

# makeCacheMatrix is function which accepts the invertible matrix as an argument, 

makeCacheMatrix <- function(x = matrix()) { 
        inv <- matrix(NA,nrow(x),ncol(x)) 
        set <- function(y){ 
                x <<- y 
                inv <- matrix(NA,nrow(x),ncol(x)) 
        }
        get <- function() x 
        setSolve <- function(solve) inv <<- solve 
        getSolve <- function() inv 
        list(set = set, get = get, setSolve = setSolve, getSolve = getSolve) 
}


# cacheSolve function accepts the special matrix created by makeCacheMatrix and checks if the inverse of the matrix is already available in cache if not it calculates
# the inverse of the matrix and puts the inverse in the cache

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        inv <- x$getSolve() 
        if(!is.na(inv[1,1])){ 
                message("getting cached data") 
                return(inv) 
        }
        data <- x$get() 
        inv <- solve(data) 
        x$setSolve(inv) 
        inv 
}
