
# The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
#1.set the value of the matrix
#2.get the value of the matrix
#3.set the value of the inverse
#4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## The inverse starts out as NULL
  i <- NULL  
  
  ## By setting the object, assign the new argument to be stored value and
  ## reset inverse calculation
    set <- function(y) {
    x <<- y
    i <<- NULL
  }
    
  ## Returns internal object
  get <- function() x
  
  ## Get and Set functions for the inverse, as above
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#The following function calculates the mean of the special "matrix" created with the "makeCacheMatrix" function.
#However, it first checks to see if the mean has already been calculated. 
#If so, it gets the mean from the cache and skips the computation. Otherwise,
#it calculates the mean of the data and sets the value of the mean in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data) ## Calculating the inverse
  x$setinverse(i)
  i
}

