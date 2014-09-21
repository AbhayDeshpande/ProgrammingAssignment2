## makeCacheMatrix creates a list of four functions- set, get, setInverse and
# getInverse.
## cacheSolve checks if there is a cached inverted matrix and solves
# the matrix if needed, setting the inverse in
# makeCacheMatrix$setInverse (invertedMatrix)

# The first function, makeCacheMatrix creates a special "matrix",
# which is a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the matrix' inverse
# get the value of the matrix' inverse

makeCacheMatrix <- function(x = matrix()) {
        invertedMatrix <- NULL
        set <- function(y) {
                cachedMatrix <<- y
                invertedMatrix <<- NULL
        }
        get <- function() x
        setInverse <- function(solvedMatrix) invertedMatrix <<- solvedMatrix
        getInverse <- function() invertedMatrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve checks for a cached inverted matrix. If there is none, it
## solves (inverts) the matrix, sets makeCacheMatrix$setInverse and returns
## the inverse of the matrix.

cacheSolve <- function(x, ...) {
        invertedMatrix <- x$getInverse()
        if(!is.null(invertedMatrix)) {
                message("getting cached data")
                return(invertedMatrix)
        }
        data <- x$get()
        invertedMatrix <- solve(data, ...)
        x$setInverse(invertedMatrix)
        invertedMatrix
}