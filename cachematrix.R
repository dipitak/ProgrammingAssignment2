## cache the inverse of a matrix function


## makeCacheMatrix:
## it is a function that creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## Initialize property
        m<-NULL
                
        ## set the matrix
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        ## get the matrix
        get <- function() {
                x
        }
        ## set the inverse of the matrix
        setInverse <- function(inverse) {
                i <<- inverse
        }
        ## get the inverse of the matrix
        getInverse <- function() {
                m
        }
        ## Return a list of the methods
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}



## cacheSolve: inverse of the special matrix returned by makeCacheMatrix(above)


## If the inverse has already been calculated and the matrix has not changed, 
## then cachesolve will be retrieved from the cache.

cacheSolve <- function(x, ...) {
        
        ## get a matrix that is the inverse of 'x'
        m <- x$getInverse()
        
        ## check if its already set
        if( !is.null(m) ) {
                message("data from cached data")
                return(m)
        }
        
        ## get matrix from our object
        data <- x$get()
        
        ## calculate the inverse using matrix multiplication
        m <- solve(data) %*% data
        
        ## Set the inverse 
        x$setInverse(m)
        
        ## get the matrix
        m      
}