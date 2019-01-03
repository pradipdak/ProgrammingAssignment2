## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 inverseM<-NULL
  
  set<-function(y) {
    x <<- y
    inverseM<<-NULL
  }
  
  get<-function() x
  setInversedMatrix<-function(newInversed)inverseM<<-newInversed
  getInversedMatrix<-function() inverseM
  list(set=set,get=get,setInversedMatrix=setInversedMatrix,getInversedMatrix=getInversedMatrix)   
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 inverseM<- x$getInversedMatrix()
  
  if(!is.null(inverseM)) {
    message("getting cached data")
    return(inverseM)
  }
  
  data<-x$get()
  inverseM<-solve(data, ...)
  x$setInversedMatrix(inverseM)
  
  inverseM       
}
