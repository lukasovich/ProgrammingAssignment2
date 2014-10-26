## Function to make "special" vector: set, get, setinv and getinv functions
## for matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                                 ## Initiate inverse as NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x         
        setinv <- function(inverse) inv <<- inverse  ## Cache inverse
        getinv <- function() inv                     ## Read cacheed inverse
        list(set = set, get = get,                   ## Return list of functions
             setinv = setinv,
             getinv = getinv)
}


## Function to calculate inverse of matrix. If cached, inverse is not calculated

cacheSolve <- function(x, ...) {
        inv <- x$getinv()     ## Read from cache
        if(!is.null(inv)) {
        ## If inverse is cached, return it without calculation    
                message("getting cached data")
                return(inv)
        }
        ## If inverse is not cached, calculate it and store to cache 
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv       ## Return matrix inverse
}