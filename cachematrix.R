## These functions cache the inverse of a matrix so that it does not need to be computed repeatedly.
##makeCachMatrix creates a list with these functions:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {   ##establish the function makeCacheMatrix with matrix x as input
  inv <- NULL         
  set <- function(y) {    ##set the value of the matix     
    x <<- y
    inv <<- NULL
  }
  get <- function() x    ## return the matrix x
  setinverse <- function(inverse) inv <<- inverse   ## store the inverse
  getinverse <- function() inv   ##return the inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) ##create list of functions
}


## This function retrieves the inverse of the matrix, or computes it if the value is not cached.

cacheSolve <- function(x, ...) {    ##establish function cacheSolve with x as input
   inv <- x$getinverse()   ##set inv equal to value of getinverse from above
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }    ##if the value of inv is not null return cached inverse
  data <- x$get()   ## set data equal to matrtix from above
  inv <- solve(data)  ##set inv  equal to the inverse of data from prior step
  x$setinverse(inv)  ##use the function from above to store inverse
  inv       ## Return inverse matrix
}
