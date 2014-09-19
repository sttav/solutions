## These functions will cache the invers of a matrix

## makeCacheMatrix makes a matrix outside its environment with the <<- operator in m

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
##setsolve stores the solve solution into m
    setsolve <- function(solve) m <-- solve
##getsolve gets value of m back 
    getsolve <- function () m
    list(set = set, get = get,
         setsolve = setsolve, getsolve= getsolve)
}

## cachesolve searches for the cached inverse matrix (solve function) by checking
## if m has a value (not NULL)
## if m has no value, it calls the solve function

cacheSolve <- function(x = matrix(), ...) {
    m <- x$getsolve()
    ## the value of m is checked to see if something is in the cache
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## if not, the solve function has to be called
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
