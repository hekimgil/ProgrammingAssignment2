## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        minv <- NULL            ## create an inverse matrix variable
        set <- function(y) {    ## getter function
                x <<- y         ## pass the value to be set
                minv <<- NULL   ## no inverse matrix set
        }
        get <- function() x     ## getter function
        setinverse <- function(solve) minv <<- solve
        getinverse <- function() minv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        minv <- x$getinverse()  ## calculate inverse and store
        if(!is.null(minv)) {    ## check to see if inverse in cache
                message("getting cached data")
                return(minv)    ## return inverse from cache
        }
        data <- x$get()         ## get data
        minv <- solve(data, ...)## calculate inverse
        x$setinverse(minv)      ## store inverse in cache
        minv                    ## output inverse in cache
}
