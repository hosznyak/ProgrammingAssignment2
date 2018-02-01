## Put comments here that give an overall description of what your functions do

## The makeCacheMatrix function works as the same logic of the makeVector function in the program assignment example.  

makeCacheMatrix <- function(x = matrix())
{
      matr_inv <- NULL
      set <- function(y)
            {
                  x <<- y
                  matr_inv <<- NULL
            }
      get <- function() x
      setinv <- function(inverse) matr_inv <<- inverse
      getinv <- function() matr_inv
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}	

## The cacheSolve function works as the same logic of the cachemean function in the program assignment example. For calculate the
## inverse, I used the solve function.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      matr_inv <- x$getinv()
      if(!is.null(matr_inv)) {
            message("getting cached data")
            return(mart_inv)
      }
      data <- x$get()
      matr_inv <- solve(data, ...)
      x$setinv(matr_inv)
      matr_inv
}

## testing the functions
matr_inp <- matrix(rnorm(36), 6, 6)
matr_inp
matr_cac <- makeCacheMatrix(matr_inp)
cacheSolve(matr_cac)

