## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Create kind of structure of cache matrix, which
## contains matrix, inverse and some methods

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    set_inv <- function(inv) inverse <<- inv
    get_inv <- function() inverse
    list(set = set, get = get, 
         get_inv = get_inv, 
         set_inv = set_inv)
}


## Write a short comment describing this function
## Functions checks if inverse matrix is cached
## if not, it calculates inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$get_inv()
    if (!is.null(inverse)) {
        message('getting cached inverse')
        return(inverse)
    }
    data <- x$get()
    x$set_inv(solve(data))
    x$get_inv()
}