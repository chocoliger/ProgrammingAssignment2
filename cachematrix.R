## makeCacheMatrix and cacheSolve allow the inverse of a square matrix to be
## solved and cached. Should the same matrix require solving afterwards, it
## can be retrieved instead of the inverse being recalculated, saving 
## processing time.

## makeCacheMatrix returns and list of functions that can be used to store a
## matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## cacheSolve accesses the solved inverse for a matrix if it has been 
## previously solved, and if not, it will calculate it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
