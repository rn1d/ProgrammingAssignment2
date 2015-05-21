## The file defines a pair of functions - `makeCacheMatrix` and `cacheSolve`,
## which allow creating a special matrix object capable of caching its
## inverse matrix for reuse, and calculating the inverse matrix, and using
## the cached value if one exists.

## It is assumed the supplied matrix is invertible.

## Usage examples:
# > a <- matrix(c(1, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3) # creates 3x3 a matrix `a`
# > b <- matrix(c(0, 1, 1, 1), 2, 2) # creates a 2x2 matrix b
# > c <- makeCacheMatrix(a) # creates a "cache matrix" object
# > d <- cacheSolve(c) # gets the inverse matrix of `a`
# > a %*% d # returns a 3x3 identity matrix, proving `d` is the inverse of `a`
# > c$set(b)  # sets c to a new matrix (2x2 matrix `b` created earlier)
# > d <- cacheSolve(c) # gets the inverse matrix of `b`
# > d %*% b # returns a 2x2 identity matrix, proving `d` is the inverse of `b`

## `makeCacheMatrix` function definition.
# The function creates a "matrix object", which is implemented as an R list.
# The elements of the list are functions, that allow performing operations
# on the internal data - the matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
    # Initializes the inverse matrix to NULL
    i <- NULL

    # Sets the matrix to the new value (overwriting the old), and resets
    # the cached inverse matrix to `NULL`
    set <- function(y) {
        x <<- y
        i <<- NULL
    }

    # Gets the matrix value
    get <- function() { x }

    # Sets/caches the inverse matrix value
    setinverse <- function(inverse) { i <<- inverse }

    # Gets the inverse matrix cached value
    getinverse <- function() { i }

    # Returns the "matrix object"
    list(set = set, get = get,
        setinverse = setinverse, getinverse = getinverse)
}

## `cacheSolve` function definition.
# Gets the inverse matrix of the passed matrix `x`. If the inverse has
# already been calculated and cached, returns the cached value. Otherwise,
# finds the inverse matrix by calling `solve(x, ...)` and caches the result
# value by calling x$setinverse.
cacheSolve <- function(x, ...) {
    # Try to get the cached inverse matrix value by calling `getinverse()`
    i <- x$getinverse()
    if (!is.null(i)) {
        # The inverse matrix is already calculated and cached
        # Return the cached value
        return(i)
    }

    # The inverse matrix has not yet been cached (or calculated)
    d <- x$get()

    # Calculate the inverse matrix for the matrix `d`
    i <- solve(d, ...)

    # Cache the result by calling `setinverse`
    x$setinverse(i)

    # Returns the inverse matrix
    i
}
