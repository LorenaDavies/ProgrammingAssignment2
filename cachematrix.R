## This function creates a special object (is not a matrix) which can cache the inverse of a matrix


makeCacheMatrix <- function(x = matrix()) {
	m <- NULL                          
	    setmatrix <- function(y) {     
				x <<- y                   
    			m <<- NULL                  	
    	}
	getmatrix <- function() x
	setinverse <- function(inverse) m<<- inverse
	getinverse <- function() m
	list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse,
  	getinverse = getinverse)  ## This creates a list of four functions

}


## This function computes the inverse of what is return from the above function. If the inverse has already been calcualted, then this function (cacheSolve) should retrieve the inverse from the funcion above (makeCacheMatrix) 

cacheSolve <- function(x, ...) {
	m <- x$getinverse() 
	if(!is.null(m)) {  
    		message ("getting cached data")
    		return(m)
    }
	 data<- x$getmatrix() 
	 m <- solve(data, ...) 
     x$setinverse(m) 
     m     ## Return a matrix that is the inverse of 'x'
}
