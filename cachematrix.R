## Assignment 2 cache inverse of a matrix

## function to create a cache for a space holder matrix

makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
       set <- function(y) {
              x <<- y
              m <<- NULL
       }
       get <- function() x
       setinverse <- function(inverse) m <<- inverse
       getmean <- function() m
       list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


## function to calculate the inverse of a squared matrix and store in cache matrix

cacheSolve <- function(x, ...) {
       m <- x$getinverse()
       if(!is.null(m)) {
              message("getting cached data")
              return(m)
       }
       data <- x$get()
       m <- solve(data, ...)
       x$setinverse(m)
       m
}
