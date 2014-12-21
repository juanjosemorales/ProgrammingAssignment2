## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a matrix object that can contain a cached matrix inverse
## returns a list of four functions to set and get a matrix and set and get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        cached.inverse <- NULL
        set <- function(y) {
                x <<- y
                cached.inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) cached.inverse <<- inverse
        getinverse <- function() cached.inverse
        list(set=set, get=get, 
             setinverse=setinverse,
             getinverse=getinverse)
}


## cacheSolve checks to see if the passed in cachedMatrixObject already had its inverse computed
## otherwise, it computes it and caches it in the cachedMatrixObject 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                return(inverse)
        }
        matrix <- x$get()
        inverse <- solve(matrix)
        x$setinverse(inverse)
        inverse
}
