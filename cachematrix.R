##There are two functions : 'makeCacheMatrix' and 'cachesolve'. 
##Computing the inverse of a matrix may be require more time so
##caching the inverse can be an useful alternative approach.


## makeCacheMatrix creates a special matrix object which can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        s<-NULL
        set<-function(y){
                x<<-y
                s<<-NULL
        }
        get<-function(){
                x
        }
        setinverse<-function(solve){
                s<<-solve
        }
        getinverse<-function(){
                s
        }
        return(s)
}


## The below function takes the cached value of the inverse matrix and skips
##its computation.

cacheSolve <- function(x, ...){
        s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}
