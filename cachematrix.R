makeCacheMatrix<- function(x = matrix) {
    i <- NULL
    #set the value of the matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    #get the value of the matriix
    get <- function() x
    
    #set the value of the inverse
    setinverse <- function(inverse) i <<- inverse
    
    #get the value of the inverse
    getinverse <- function() i
    
    #assign functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

cacheSolve<- function(x, ...) {
    i <- x$getinverse()
    
    #checkcache for existing value of inverse
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    #get matrix
    data <- x$get()
    
    #calculate inverse
    i<-solve(data)
    
    #set value of inverse in cache
    x$setinverse(i)
    
    #return innverse
    return i
}
