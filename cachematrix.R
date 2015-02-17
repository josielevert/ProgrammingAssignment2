## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its reverse

makeCacheMatrix <- function(x = numeric()) { ##set the value of the matrix
      i <- NULL
      set <- function(y) {
              x <<- y
              i <<- NULL
      }
      get <- function() x ##get the value of the matrix
      setInverse <- function(solve) i <<-inverse  ## set the value of the inverse matrix
      getInverse <- function() i                  ## get the value of the inverse matrix
      list(set = set, get = get, setInverse = setInverse, getInverse= getInverse)
}



##Computes the inverse of the special "matrix" retuned by makeCacheMAtrix 
##after checking if calculated before

cacheSolve <- function(x, ...) {
       
        i <- x$getInverse()  ##checks if inverse is already calculated
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...) ## calculate unverse
        x$setInverse(i)
        i
}
        


