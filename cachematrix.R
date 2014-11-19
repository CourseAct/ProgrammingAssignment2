## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

## This function creates a special matrix object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}
## This function accesses the value if it has been created
## If the object has not been created it creates it

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  ##if the inverse exists return it  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##inverse not calculate, so do it 
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  ##Return the value
  return(m)
}
##mat<-matrix(1:4,2,2)
##bigVec <- makeCacheMatrix(mat) 
## cacheSolve(bigVec)
