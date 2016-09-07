
## List of functions for cache-ing matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(sol) m <<- sol
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}



## Calculate inverse if it has not been calculated before

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    dat <- x$get()
    m <- solve(dat)
    x$setinv(m)
    m        
}

#Test
#abc <- matrix(1:4,2,2)
#yz<- makeCacheMatrix(abc)
#cacheSolve(yz)
