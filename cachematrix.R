## Put comments here that give an overall description of what your
## functions do
# use makeCacheMatrix to initialize a matrix and then use cacheSolve to get inverse matrix
# and then saved it.
## Write a short comment describing this function
# makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inverse<- NULL
        set <- function(y) {
                x <<- y
                inverse<<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse<<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
# cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse<- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse<- solve(data,...)
        x$setinverse(inverse)
        inverse
}
