## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by   
##             makeCacheMatrix above. If the inverse has already been calculated 
##             (and the matrix has not changed), then the cachesolve should retrieve the 
##             inverse from the cache.


## function makeCacheMatrix creates a special matrix object
## calculates the inverse of the matrix.
## If the matrix inverse has already been calculated, it will instead 
## find it in the cache and return it, and not calculate it again.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  ## calculates the inverse of the matrix and cache
  setmatrix<-function(solve) m<<- solve
  ## get inverse of the matrix
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)

}


## The function cacheSolve returns the inverse of a matrix A created with
## the makeCacheMatrix function.
## If the cached inverse is available, cacheSolve retrieves it, while if
## not, it computes, caches, and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getmatrix()
  ##  if cached inverse matric available, return it
  if(!is.null(m)){
    message("getting cached inverse matrix data")
    return(m)
  }
  ##  if cached inverse matric not available, calculate inverse,cache it and return
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
