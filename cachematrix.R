## Programming Assignment 2 for R Programming Course
## By DarseZ, Aug 2019


makeCacheMatrix <- function(x = matrix()) {
    ## Create a special "matrix" object that cache its inverse
    Inv <- NULL
    set <- function(y){
        x <<- y
        Inv <<- NULL
    }
    get <- function() x
    setInverse <- function(Inverse) Inv <<- Inverse
    getInverse <- function() Inv
    
    list(set = set, get = get, setInverse = setInverse, gemtInverse = getInverse)

}


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    Inv <- x$getInverse()
    if(!is.null(Inv)) {
        message("Getting cached data")
        return(Inv)
    }
    
    data <- x$get()
    Inv <- solve(data, ...)
    x$setInverse(Inv)
    Inv
    
}
