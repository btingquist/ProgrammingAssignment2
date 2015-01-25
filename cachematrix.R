## BKT 24-Jan-2015
## Two functions that work together to  determine the inverse of an 
## invertible matrix and cache the results in case of repeated/looped
## computations. 
##


## Creates a special "matrix object that can cache its inverse.
## This function call creates a list that contains four functions that
## are called from a complementary function named cacheSolve.

makeCacheMatrix <- function(x = matrix()) 
{
     m <- matrix() ## initiate a local variable m that is empty

     set <- function(y) {
          x <<- y
          m <<- matrix()
     }

     get <- function() x
     
     setinv <- function(inverse) m <<- inverse
     
     getinv <- function() m
     
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}



## This function computes the inverse of the results returned from the 
## makeCacheMatrix function above. When the same matrix is called, the function 
## returns results from the cache. Otherwise, a new result is calculated and 
## stored in the cache.

cacheSolve <- function(x, ...) 
{    ## Return a matrix that is the inverse of 'x'
     
     m <- x$getinv()
     if (any(is.na(m))==FALSE)
     {
          message("getting inverted matrix from cache")
          return(m)
     }     

     data <- x$get()
     m <- solve(data, ...)
     x$setinv(m)
     m
     
}
