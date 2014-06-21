## To get the Inverse of a Matrix
## here is created a list of functions that can get the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  invMatriX <- NULL
  set <- function(M) {
    x <<- M
    invMatriX <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) invMatriX <<- inverse
  getinverse <- function() invMatriX
  
  list(set=set, 
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

## The next lines of code will computes the inverse of the matrix
## returned by makeCacheMatrix(), if the inverse has
## already been calculated, then it retrieves from the PC cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMatriX <- x$getinverse()
  
  if(!is.null(invMatriX)) {
    message("already getting cached data.")
    return(invMatriX)
  }
  
  data <- x$get()
  invMatriX <- solve(data)
  x$setinverse(invMatriX)
  invMatriX
}

## Test case
##> x = rbind(c(1, 2), c(3, 4))
##> H = makeCacheMatrix(x)
##> H$get()

##     [,1] [,2]
##[1,]    1    2
##[2,]    3    4

##> cacheSolve(H)
##     [,1] [,2]
##[1,] -2.0  1.0
##[2,]  1.5 -0.5

##> k <- H$get()
##> j <- cacheSolve(H)
##already getting cached data.
## to test make the matrix product
##> k %*% j
##     [,1]         [,2]
##[1,]    1 1.110223e-16
##[2,]    0 1.000000e+00

## Jejeje...the test of the product of a matrix with its inverse, the result
## must be the matrix identity (all diagonal elements are 1, and the others ceros), but is 
## incredible this result, because one element of this product's matrix, in particular
## 1.110223e-16 is almost cero, but this element never is equal to cero after all.


