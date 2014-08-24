## Put comments here that give an overall description of what your
## functions do

## caches the value of inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
m <- matrix(c(0,0,0,0),ncol=2,nrow=2)
  set <- function(y) {
    x <<- y
    m <<- matrix(c(0,0,0,0),ncol=2,nrow=2)
  }
  get <- function() {x}
  setinverse <- function(solve) { m <<- solve}
  
  getinverse <- function() {m}
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)


}


## finds the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
  if(colSums(t(m)%*%m) !=0 && rowSums(t(m)%*%m)!=0) {
    message("getting cached data")
    return(m)
  
  }else 
 { data <- x$get()
   m <- solve(data, ...)
   x$setinverse(m)
   m
   
}
}
