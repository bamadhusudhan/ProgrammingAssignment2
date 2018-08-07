## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <- y
                inv <- NULL
        }
        get <- function() x
        setinv <- function(z) inv <<- z
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
# following lines of code are for testing and to illustrate where lexical scoping is useful
#d1 <- matrix(data = rexp(25, rate = 10), nrow = 5, ncol = 5)
#d2 <- matrix(data = rexp(25, rate = 10), nrow = 5, ncol = 5)
#t1 <- makeCacheMatrix(d1)
#t2 <- makeCacheMatrix(d2)
#cacheSolve(t1)
#cacheSolve(t1)
# the second time the above ine is called - data is fetched from the cache
#cacheSolve(t2)
#cacheSolve(t2)
#as many instances of this function can be created and once the inverse is calculated, they are stored in the cache

