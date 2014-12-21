## Matrix inversion is computational expensive.
## This module allow to create matrix that caches its inverse in order
## to eliminate redundant evaluations

# Creates an special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL           # if exists, cached value of inverse
    
    set <- function(y){
        x <<- y         # saves matrix to this function's environment
        inv <<- NULL      # invalidates cached inverse 
    }
    
    get <- function() x                 # returns matrix
    setinverse <- function(inverse) inv <<- inverse  # caches the inverse
    getinverse <- function() inv          # if it exists, returns cached inverse
    
    # returns a list of functions of the matrix and its inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Computes the inverse of the special matrix returned by 
## makeCacheMatrix function. If the inverse has already been calculate 
## and the matrix has not changed, then this function should retrieve
## the inverse from the cache
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()      # checks if inverse is cached
        if(!is.null(inv)){
            message("getting cached data")
            return(inv)
        }
        matrix <- x$get()          # gets matrix
        inv <- solve(matrix, ...)  # calculates inverse of matrix
        x$setinverse(inv)          # stores the inverse
        inv                        # returns inverse
}
