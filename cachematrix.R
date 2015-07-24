##The following two functions return the inverse of a matrix.
##Matrix [A] is the inverse of Matrix [B] if and only if
##[A][B] AND [B][A] return the identity matrix.

##makeCacheMatrix creates a list containing a function to
##1. set the value of the matrix
##2. get the value of the matrix
##3. set the value of the inverse matrix
##4. get the value of the inverse matrix
## basically serves as a directory

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ##sets matrix
  set <- function(y) {
    x <<- y                             ##double arrows due env crossing
    inv <<- NULL
  }

  ##gets matrix
  get <- function()x
  
  ##sets inverse
  setinverse <-function(inverse) inv <<- inverse
    
  ##gets inverse
  getinverse <- function() inv
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

##The next function actually returns the inverse matrix.
##  First, it checks whether the inverse has already been computed
##  by looking throuch cached data--if nothing is found, it computes
##  the inverse and returns it.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)){       ##sees if there is an inv value
          message("Getting Inverse from Cache")
          return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

