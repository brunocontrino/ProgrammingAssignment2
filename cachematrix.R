## 
## Function makeCacheMatrix makes a chached matrix 'x' as seen in vector example.
## Function cacheSolve finding the inverse of the chached matix 'x' using solve().


makeCacheMatrix <- function(x = matrix()) {
	  s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) s <<- solve
        getmatrix <- function() s
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


}



cacheSolve <- function(x, ...) {
	  s <- x$getmatrix()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$getmatrix()
        s <- solve(data, ...)
        x$setmatrix(s)
        s
}
        
}
