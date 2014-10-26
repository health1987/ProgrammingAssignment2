## get cached data

## get cached data when you generated data once
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL # initialize m
        ## resize the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x  # get the input matrix
        setmatrix <- function(matrix) m <<- matrix  # set matrix to cached environment
        getmatrix <- function() m  # get matrix from cached environment
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

## solve the matrix
cacheSolve <- function(x, ...) {
        m <- x$getmatrix()  # get matrix from cached environment
        ## judge whether cached is empty
        if(!is.null(m)) {
                message("getting cached data")
                return(m)  # output the matrix if cached is not empty
        }
        data <- x$get()  # get the input matrix
        m <- solve(data, ...)  # solve matrix
        x$setmatrix(m)  # set matrix to cached environment
        m
}
