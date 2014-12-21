## Caching the inverse of a matrix
## makeCacheMatrix creates a special matrix object, and then 
## cacheSolve caches its inverse

## The first function, makeVector creates a special "matrix", which is really a list containing a function to##
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

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
## The function cacheSolve returns the inverse of a matrix created with
## the makeCacheMatrix function.
## If the cached inverse is available, cacheSolve retrieves it, while if
## not, it computes, caches, and returns it.

cacheSolve <- function(x=matrix(), ...) {
        m<-x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix <- x$get() 
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m
}