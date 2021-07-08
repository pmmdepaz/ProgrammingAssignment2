## The two elements of the content are makeCacheMatrix and cacheSolve
## The function makeCacheMatrix comprises of set, get, setInverse, and getInverse 


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
                }
        get <- function() {x}
        setInverse <- function(inverse) {inv <<- inverse}
        getInverse <- function() {inv}
        list(set = set, 
             get = get,
             setInverse + setInverse, 
             getInverse = getInverse)
        }
      

## cacheSolve, the function used in this section, solves for the inverse of the cacheMatrix function.
## If the inverse is already solved, it retrives the inverse from the cache. 

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
        
}
