
## Programming assignment 2 (Daniel Ravaglia)
## The makeCacheMatrix function creates a "matrix" and caches its inverse.

makeCacheMatrix <- function(x = matrix()) 
	  {
        m <- NULL
        set <- function(y) 
	  {
                x <<- y
                m <<- NULL
        }
        ## the matrix is cached here.
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
	  }


## The cacheSolve function calculates the mean of the 
## special "vector" created with the above function.

cacheSolve <- function(x, ...) 
	  {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if (!is.null(m)) 
	  	{
            	    message("getting cached data")
                	return(m)
        }
        ## this is the step that actually gets the inverse -- 
        mat <- x$get()
        m <- solve(mat, ...)
        x$setInverse(m)
        m
	}


## this is the test case I used to verify the function. 
print ("example 1")
x = rbind(c(1, 1/2, -7), c(3, 7/11, 1), c(3, 5, 13))
x
solve(x)
