## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	## x is the input which is an invertible matrix
	invm <- NULL
	set <-function(y) {
		x<<-y
		invm<-NULL
		}
	
	get <- function() x
	setinverse <- function(solve) {
	invm<<-inverse
	}
	getinverse <- function() {
		invm
		}
	list(set=set, get = get,
		set=set,
		get=get)
	}

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invm <- x$getinverse()
        if(!is.null(invm)) {
        	message("getting cached data")
        	return(invm)
        }
        data <-x$get()
        invm<-solve(data,...)
        x$setinverse(invm)
        invm
}		
