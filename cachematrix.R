## This program will first make a special "matrix" to cache the result of
#a matrix inverse and then will get or cacluate the matrix inverse and store
#it in the cache.

#makeCacheMatrix creates a special "matrix" and containis a function 
#that sets and gets the value of the matrix and sets and gets
#the value of matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


#cacheSolve returns a matrix that is the inverse X. 
#First the function checks to see if the matrix inverse has 
#been calculated and, if so, gets the matrix inverse from the cache 
#and skips the cacluation. If there is no matrix inverse in the cache, 
#the function calculates the matrix inverse of x and sets the 
#result in the cache.

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

