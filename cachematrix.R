## The makecacheMatrix will inverse the matrix,called x, and put 
##the inverse matrix in inv, which can be called by the cacheSolve matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setinv <- function(solve) {inv <<- solve}
        getinv <- function() {inv}
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## the cacheSolve will call the inverse matrix. It returns it if 
## it is calculated already, or calculate it if it is called for the 
##first time.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
