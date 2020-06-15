## These functions allow the user to cache the inverse of a matrix 
## so that it can be looked up in the cache rather than recomputed


## makeCacheMatrix creates a list containing a function to
      ##  1. set the value of the matrix
      ##  2. get the value of the matrix
      ##  3. set the value of the inverse matrix
      ##  4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
      inv<-NULL
      set<- function(y){
      x<<-y
      inv<<-NULL
      }
      get<-function() x
      setinv<-function(inverse) inv<<-inverse 
      getinv<-function() inv
      list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve calculates the inverse of the matrix stored in the 
## list created with the above function. However, it first checks
## to see if the inverse has already been calculated. If so, it gets 
## the inverse from the cache and skips the computation. Otherwise, it 
## calculates the inverse of the data and sets the value of the inverse
## in the cache via the setinv function.

cacheSolve <- function(x, ...) {
      inv<-x$getinv()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      data<-x$get()
      inv<-solve(data, ...)
      x$setinv(inv)
      inv
}
