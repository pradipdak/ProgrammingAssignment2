## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  set <-function(y) {
    x<<-y
    inverse<<-NULL
  }
  get<-function() x
  setInversed<-function(newInversed) inversed <<- newInversed
  getInversed<-function() inversed
  list(set=set,get=get,setInversed=setInversed,getInversed=getInversed)      
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 inverse <- x$getInversed()
  
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInversed(inversed)
  
  inverse
}
