## To create a pair of functions that can cache the inverse 
## of a matrix rather than compute it repeatedly.

## The first function makeCacheMatrix would create a special matrix 
## object that can cache the input matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  ## sets the value of m to NULL
             ## to provide a default if cacheSolve has't yet be used
  
  set <- function(y) { ## sets the value of the matrix
    x <<- y   ## caches the inputted matrix so that cacheSolve
              ## can check whether it has changed
    m <<- NULL  ## sets the value of m to NULL
  }
  
  get <- function() x
  setinverse <- function(solve) m <<- solve ## sets the value of 
                                            ## the inverse matrix
  getinverse <- function() m
  list(set=set, get=get, ## creates a list contains the four functions
       setinverse=setinverse,
       getinverse=getinverse)
  
}

## The second function cacheSolve calls functions stored in 
## the special "matrix" returned by the first function makeCacheMatrix


cacheSolve <- function(x=matrix(), ...) {
  m <- x$getinverse()  ## if an inverse has already been calculated
                       ## then gets the inverse from the cache
  
  if(!is.null(m)){  ## checks to see if cacheSolve has been run before
    message("getting cached data") 
    return(m)
  }
    matrix <- x$get() ## gets the value of the input matrix

    m <- solve(matrix, ...) ## calcuates the value of the inverse
                            ## of the input matrix
  
    x$setinverse(m) ## runs the setinverse function on the inverse
                    ## to cache the inverse
  
    m  ## Return a matrix that is the inverse of 'x'
}
