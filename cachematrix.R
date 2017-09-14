##  In order to prevent computing the inverse of a matrix,
## we can write some funcitons caching the inverse of a matrix
## 


## This function creates a special "Matrix",which returns a list
## containing functions,like set,get.
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inverse<<- inverse
        getinverse <- function() inverse 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## the funciotn calculates the inverse of a matrix using 
## the cached matrix created by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached matrix")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
