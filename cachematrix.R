## The two functions examine a given matrix.  If the inverse of that
## matrix has not been calculated it calculates the inverse. If the 
## Inverse has been calculated then it reads the inverse from memory.

## The 'makeCacheMatrix' function returns a list of  4 functions.
## The 'set' function assigns a matrix while the 'get' function
## outputs the matrix after it has been assigned. 
## Similarly the 'setinverse' function assings the inverse to memory
## after it has been calculated and the 'getinverse' function retreives
## the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  
  ### Assigns a null value to the inverse variable i.
  i <- NULL
  ### Assigns a matrix value to matrix variable x and clears out the
  ### invers variable y
  set <- function(y){
    x <<- y
    i <<- NULL
    
  }
  ### Retreives the matix value from the variable.
  get <- function() x
  ### Assigns an inverse value to an inverse variable. 
  setinverse <- function(inverse) i <<- inverse
  ### retreives the inverse value from the variable.
  getinverse <- function() i
  list(set = set, get = get, 
       setinverse = setinverse, getinverse = getinverse)

  
}


## The function 'cacheSolve' checks to see if the inverse of a matrix
## x exists in memory.  If it does then it returns the inverse.
## If it does not then it calulates the inverse and returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
