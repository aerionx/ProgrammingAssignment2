## makeCacheMatrix is a function that stores a matrix "X" in the memory
## cacheSolve is a function that gives matrix  inverse 



## This function stores matrices in memory using scoping rules

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      
      # Matrix setter
      set <- function(y) {
       
        x <<- y
        i <<- NULL
      }
      
       # In order to get the matrix...
      get <- function() x
      setinverse <- function(inverse) i <<- inverse
      getinverse <- function() i
      # The following returns the matrix re-arranged
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function returns the inverse of a matrix A created with the makeCacheMatrix function . 
## If the cached inverse is available it retrieves it. If not, computes, caches, and returns it.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
      i <- x$getinverse()
      #message delivered if there is any previous cache
      if(!is.null(i)) {
        message("retriving information from cache")
        return(i)
      }
      #if there is no previous cahce we need to calculate it...
      data <- x$get()
      i <- inverse(data, ...)
      x$setinverse(i)
      i
}

##Examples

##  > a <- matrix (1:4,2,2)
##  > CachedA <- makeCacheMatrix(a)
##  > cacheSolve(CachedA)
##     [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##  > cacheSolve(CachedA)
## retriving information from cache
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5



##  > a <- diag (20,10)
##  > a
##      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
## [1,]   20    0    0    0    0    0    0    0    0     0
## [2,]    0   20    0    0    0    0    0    0    0     0
## [3,]    0    0   20    0    0    0    0    0    0     0
## [4,]    0    0    0   20    0    0    0    0    0     0
## [5,]    0    0    0    0   20    0    0    0    0     0
## [6,]    0    0    0    0    0   20    0    0    0     0
## [7,]    0    0    0    0    0    0   20    0    0     0
## [8,]    0    0    0    0    0    0    0   20    0     0
## [9,]    0    0    0    0    0    0    0    0   20     0
##[10,]    0    0    0    0    0    0    0    0    0    20
##  > CachedMatrix <- makeCacheMatrix(a)
##  > cacheSolve(CachedMatrix)
##      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
## [1,] 0.05 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00  0.00
## [2,] 0.00 0.05 0.00 0.00 0.00 0.00 0.00 0.00 0.00  0.00
## [3,] 0.00 0.00 0.05 0.00 0.00 0.00 0.00 0.00 0.00  0.00
## [4,] 0.00 0.00 0.00 0.05 0.00 0.00 0.00 0.00 0.00  0.00
## [5,] 0.00 0.00 0.00 0.00 0.05 0.00 0.00 0.00 0.00  0.00
## [6,] 0.00 0.00 0.00 0.00 0.00 0.05 0.00 0.00 0.00  0.00
## [7,] 0.00 0.00 0.00 0.00 0.00 0.00 0.05 0.00 0.00  0.00
## [8,] 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.05 0.00  0.00
## [9,] 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.05  0.00
## [10,] 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00  0.05
##  > cacheSolve(CachedMatrix)
##  retriving information from cache
##      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
## [1,] 0.05 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00  0.00
## [2,] 0.00 0.05 0.00 0.00 0.00 0.00 0.00 0.00 0.00  0.00
## [3,] 0.00 0.00 0.05 0.00 0.00 0.00 0.00 0.00 0.00  0.00
## [4,] 0.00 0.00 0.00 0.05 0.00 0.00 0.00 0.00 0.00  0.00
## [5,] 0.00 0.00 0.00 0.00 0.05 0.00 0.00 0.00 0.00  0.00
## [6,] 0.00 0.00 0.00 0.00 0.00 0.05 0.00 0.00 0.00  0.00
## [7,] 0.00 0.00 0.00 0.00 0.00 0.00 0.05 0.00 0.00  0.00
## [8,] 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.05 0.00  0.00
## [9,] 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.05  0.00
##[10,] 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00  0.05
