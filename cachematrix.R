## These two functions will store the inverse of a matrix object (given that it is 
## invertable) in the cache, in order to save time and memory usage in the 
## case of complex operations

## makeCacheMatrix will store a defined matrix in the cache. For best results, set a
## variable to the result of the function: "step1 <- makeCacheMatrix()".

makeCacheMatrix <- function(x = matrix()){
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get  <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function () m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve will check for the inverse of the matrix stored in the cache, and if it
## is not present or has changed, cacheSolve will compute and return the inverse.
## Use with the above variable stored from the previous function: "cacheSolve(step1)".

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
## Happy inverting!!!