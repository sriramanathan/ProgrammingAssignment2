## Assignment: Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation 
## There are benefits of caching the inverse of a matrix than compute it repeatedly 
## Below are the pair of functions that cache the inverse of a matrix.

## The first function, makeCacheMatrix, creates a special "matrix" object that can cache its inverse
## set the value of the matrix - set() function
## get the value of the matrix - get() function
## set the value of the inverse matrix - setinverse() function 
## get the value of the inverse matrix - getinverse() function

makeCacheMatrix <- function(x = matrix()) {
     n <- NULL 
     set <- function(y) {
          x <<- y         
          n <<- NULL    
     }
     get <- function() x  
     setinverse <- function(inverse) n <<- inverse 
     getinverse <- function() n 
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
}

## The second function, cacheSolve, computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve will retrieve the inverse from the cache.
## solve() is used to return the inverse of the matrix

cacheSolve <- function(x, ...) {
     n <- x$getinverse() 
     if(!is.null(n)) {
          message("getting cached data")
          return(n)
     }
     data <- x$get()
     n <- solve(data,...)
     x$setinverse(n)
     n
}

## Code End