## This function creates a special "matrix" object that can 
## cache its inverse

## Setting and getting the matrix and the getting and setting the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv=NULL
    set=function(y){
        x<<-y
        inv<<-NULL
    }
    get=function()x
    setinv=function(inverse) inv<<-inverse
    getinv=function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function computes the cache inverse of the matrix returned 
## makeCacheMatrix- should retrieve the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv=x$getinv()
    
    ## Get cached data if available
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    ## if not available, calculate the inverse
    mat.data=x$get()
    inv=solve(mat.data,...)
    
    x$setinv(inv)
    
    return(inv)
}
