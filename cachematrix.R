## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL        ## initializing the inverse as NULL 
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x        ## function to get matrix x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)){        ## checking whether the inverse is NULL
        message('getting cached data!')
        return(inv)        ## returning the inverse
    }
    data <- x$get()
    inv <- solve(data, ...)        ## calculating the inverse
    x$setinv(inv)
    inv ##return a matrix that is the inverse of 'x'
}