## Put comments here that give an overall description of what your
## functions do
## These functiosn allow us to record the caluclation of the inverse of a matrix in another environment, 
## and to retrieve this inverse whenever we need it without having to recalulate it

## Write a short comment describing this function
##The first function creates a list of four functions to set the value of the matrix, get the value of the matrix, set the value of the inverse, get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
## The function calculates the inverse of the matrix. However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
        ## Return a matrix that is the inverse of 'x'
}
