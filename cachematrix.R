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

makeCacheMatrix <- function(x = matrix()) { # x is an invertible matrix
        inv <- matrix(NA,nrow(x),ncol(x)) # inv is an matrix for holding cached values all elements are set to NA
        set <- function(y){ 
                x <<- y # setting an invertible matrix to x for x to be used in different function 
                inv <- matrix(NA,nrow(x),ncol(x)) # inv is an matrix for holding cached values all elements are set to NA, while setting up the matrix
        }
        get <- function() x #return the invertible matrix supplied to makeCacheMatrix function as argument
        setSolve <- function(solve) inv <<- solve #calculating the inverse of an invertible matrix and assigning the value to inv function
        getSolve <- function() inv #returning the inv variable as inverse of the invertible matrix
        list(set = set, get = get, setSolve = setSolve, getSolve = getSolve) # creating a list of actions assigned in the makeCacheMatrix function
}


# cacheSolve function accepts the special matrix created by makeCacheMatrix and checks if the inverse of the matrix is already available in cache if not it calculates
# the inverse of the matrix and puts the inverse in the cache

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        inv <- x$getSolve() # Getting the inverse of the matrix and setting it to inv variable
        if(!is.na(inv[1,1])){ # if inverse is available inv[1,1] value wouldn't be NA this logic checks that
                message("getting cached data") # confirming we are getting the value from the cache
                return(inv) # returning the value of inverse of the matrix if the value available in the cache
        }
        data <- x$get() # assigning the matrix to data variable to be used for calculating the ivnerse of the matrix
        inv <- solve(data) # setting up the inv variable to be used in cache function
        x$setSolve(inv) # setting the recently set inverse to cache
        inv # returing the inverse of the matrix
}
