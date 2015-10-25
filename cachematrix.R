## This creates two functions that cache a square input matrix and 
## its inverse (makeCacheMatrix function) and computes the inverse 
## of a square matrix and stores it (cathces it) to save 
## computation time.


## makeCacheMatrix function creates a special "matrix" object that can cache the input 
## matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
 
  set <- function(z) {
    
      x <<- z
      inv <<- NULL
      
  }

  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  
  list(set = set, get = get,
       setinv = setinv, 
       getinv = getinv)
  
}



## cacheSolve function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix funcion above (or retrieves the inverse from the cache
## if it has been already calculated and cached)

cacheSolve <- function(x=matrix(), ...) {
  
  inv <- x$getinv()
  if(!is.null(inv)) {
      
        message("getting cached data")
        return(inv)
    
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  
}
