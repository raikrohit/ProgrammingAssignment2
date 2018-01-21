## makeCacheMatrix cache's the inverse of a matrix in memory
## cacheSolve actually looks for the cached value and passes the inverse value on prompt
## <<- denotes a value cached 


## makeCacheMatrix function is meant to be reference cached function to set, get and 
## return inverse value of the matrix

makeCacheMatrix <- function(x = matrix()) 
{
                m <- NULL
        	set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function looks for any cached value of matrix using x$getinvserse
## if the value is NULL it calculates the inverse value using solve function and 
## references get(),setinverse().

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
