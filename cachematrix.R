makeCacheMatrix <- function(x = matrix()) {
  
  i<-NULL
  set = function(y) {

    x <<- y
    i <<- NULL
  }
  get <- function() x
  #inverse of the matrix 
  setinv<- function(inverse) i <<- inverse 
  getinv <- function() i
  list(set=set, get=get, 
       setinv=setinv, 
       getinv=getinv)
}


#solve the cache matrix 
cacheSolve <- function(x, ...) {
  
  i<- x$getinv()
  
  if (!is.null(i)){
    
    message("getting cached data")
    return(i)
    
  }
  
  mat<- x$get()
  i <- solve(mat, ...)
  x$setinv(i)
  return(i)
}

testMatrix <-matrix(c(2,3,0,2),nrow=2)
matr<-makeCacheMatrix(testMatrix)
cacheSolve(matr)
cacheSolve(matr)

