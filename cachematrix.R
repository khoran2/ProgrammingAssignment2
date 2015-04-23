##this function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## making environment 
  m <- NULL
  ## create some functions
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  ## set inverse
  get <- function() x
  setinverse <- function(inverse) m <<-inverse
  getinverse <- function() m
  ## return the functions
  list(set = set, get = get, 
  setinverse = setinverse,
  getinverse = getinverse)
}
test <- matrix( c(1,2,3,4), nrow=2, ncol=2, byrow=T)
MyMatrix <- makeCacheMatrix(test)
MyMatrix$get()

## this function computes the inverse ofr the special matrix

cacheSolve <- function(x, ...) {
        ## Returning a matrix that is the inverse of 'x'
    m <- x$getinverse()
    ## remove null and pass message to user
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    ## return the functions
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}

x <- matrix( c(1,2,3,4), nrow=2, ncol=2, byrow=T)


