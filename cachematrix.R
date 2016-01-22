## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	## x is the input which is an invertible matrix
	invm <- NULL
	set <-function(y) {
		x<<-y
		## the <<- operator which can be used to assign a value to an object in an 
		## environment that is different from the current environment
		invm<-NULL
		}
	get <- function() x
	setinverse <- function(inverse) 
	invm<<-inverse
	
	getinverse <- function() 
		invm
		
	list(set=set, get = get,
		setinv=setinv,
		getinv=getinv)
	}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invm <- x$getinverse()

        if(!is.null(invm)) {
        ## Inverse matrix is in cached data, no need for calculation   
        	message("getting cached data")
        	return(invm)
        }
        ## calculation is required to return the inverse of the matrix
        data <-x$getinv()
        invm<-solve(data,...)
        ## value of the inverse matrix is stored in the cache
        x$setinv(invm)
        invm
}		
