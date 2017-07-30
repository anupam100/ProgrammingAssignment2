## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{
  inver <- NULL                             
  set <- function(y) 
  {                    
    x <<- y                             
    inver <<- NULL                         
  }
  get <- function() x                     
  
  setinverse <- function(inverse) inver <<- inverse
  getinverse <- function() inver                     
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  
  inv1 <- x$getinverse()
  if(!is.null(inv1)) 
  {
    message("get cached data")
    return(inv1)
  }
  data <- x$get()
  inv1 <- solve(data, ...)
  x$setinverse(inv1)
  inv1
}
