## ==================================
## Pierpaolo Cacciotto - Assignment 2 
## ==================================
## The following functions first create a special "matrix" that will be used to cache its 
## inverse. Then, the actual inverse is computed. As a final check the matrix product is 
## computed.
## ==================================
## The following function create a matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	 m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
 ## Set the value of the inverse
        setInverse <- function(inverse) m <<- inverse
 ## Get the value of the inverse
        getInverse <- function() m
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## ==================================
## The following function computes and return the inverse matrix of X. 
## First check if the inverse has already been calculated 
## It also prints the value of the original matrix X and the matrix product as a check
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	  ## Get the original matrix X and print it (optional)
	   message("Input matrix")
	   y <- x$get()
	   print(y)
	  ## Get the inverse matrix of X
       m <- x$getInverse()
      ## ====
	  ## Check if the inverse has been already calculated.
		if (!is.null(m)) {
        	        message("Inverse matrix (from cache)")
            	    print(m)
                ## Compute the matrix product X*M as a check (X*M = I) - optional
        			i <- m%*%y
       				message("Final check")
        			print(i)
        	} else {
   			    ## If not then compute the inverse matrix with solve()	
        			m <- solve(y, ...)
        			x$setInverse(m)
        			message("Inverse matrix (computed)")
        			print(m)
                ## Compute the matrix product X*M as a check (X*M = I) - optional
        		 	i <- m%*%y
       				message("Final check")
   					print(i)
        	}
}
