## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function that stores a matrix "X" in the memory
## cacheSolve is a function that gives matrix  inverse 


## Write a short comment describing this function
## stores matrices in memory using scoping rules

makeCacheMatrix <- function(X = matrix()) {
inverse <- NULL
set <- function(Y){
	X <<- Y
	inverse <<- NULL
	}
get <- function() X
setinverse <- function(Inverse) inverse <<- Inverse
getinverse <- function() inverse
list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## Write a short comment describing this function
##Returns the inverse of a matrix A created with the makeCacheMatrix function . If the cached inverse is available it retrieves it. If not, computes, caches, and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
