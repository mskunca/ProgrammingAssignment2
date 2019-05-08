## general description
## makeCacheMatrix - creates a "special matrix" needed for cacheSolve
## cacheSolve - reads inverse from cache or calculates and saves it
##
## function makeCacheMatrix creates a "special matrix" 
## x <- makeCacheMatrix(matrix(rnorm(1000000, mean=5, sd=20),nrow=1000,ncol=1000))
## x$get gets "special matrix" values
## x$setmatrinv sets "special matrix" inverse
## x$getmatrinv gets "special matrix" inverse if stored
## push to the parent environment x <<- y to be accessible by cacheSolve function
## 


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrinv <- function(matrinv) m <<- matrinv
        getmatrinv <- function() m
        list(set = set, get = get,
             setmatrinv = setmatrinv,
             getmatrinv = getmatrinv)
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrinv(m)
        m
}



## math check
## y <- cacheSolve(x) 
## z <- x$get()
## z %*% y
## Correct if Last expression yields Identity matrix