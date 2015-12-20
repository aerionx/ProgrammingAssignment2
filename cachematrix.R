## makeCacheMatrix is a function that stores a matrix "X" in the memory
## cacheSolve is a function that gives matrix  inverse 



## This function stores matrices in memory using scoping rules

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
       
        x <<- y
        m <<- NULL
      }
      
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function returns the inverse of a matrix A created with the makeCacheMatrix function . 
## If the cached inverse is available it retrieves it. If not, computes, caches, and returns it.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
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
