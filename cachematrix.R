#These functions cache potentially time-consuming computations. 
#If the contents of a matrix are not changing, it may make sense to cache the value of the inverse so that when we need it again, it can 
#be looked up in the cache rather than recomputed. These functions take advantage of the scoping rules of the R language to preserve 
#state inside of an R object.

# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

#The following function calculates the inverse of the special "matrix" created with the above function.
#It first checks to see if the inverse has already been calculated. 
#If this operation has already been performed, it gets the inverse from the cache.
#Otherwise, it calculates the inverse of the data and sets the value of the inverse
#in the cache via the setinverse function.Returns a matrix that is the inverse of 'x'


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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