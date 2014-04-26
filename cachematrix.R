## File contains two functions: 
## 1) makeCacheMatrix - special type of "matrix" which contains a cached 
## version of inversed matrix in it (if may be set to it manually or by 
## second function "cacheSolve")
## 2) cacheSolve - invert special type of "matrix" which is created by
## makeCacheMatrix function and updates inner state of makeCacheMatrix matrix
## by setting the value to it (makes a cache)

## create a special "matrix" which contains matrix value in it
## and also cached version of inversed matrix if it is not null
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolution <- function(solution) s <<- solution
    getsolution <- function() s
    list(set = set, get = get,
         setsolution = setsolution,
         getsolution = getsolution)
}


## Return a matrix that is the inverse of 'x'
## also caches it inside x "matrix"
cacheSolve <- function(x, ...) {
    s <- x$getsolution()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolution(s)
    s
}