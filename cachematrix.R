##
## makeCacheMatrix is a factory function that creates a structure
## to hold a matrix, compute its inverse, and store the inverse for
## future reference. Inversion is done using the base package solve()
## function. Errors are not caught.
##
## cacheSolve computes (or fetches from cache) the inverse of a matrix
## stored in a "CacheMatrix" instance.
## 

##
## Given a matrix (possible null), create a "CacheMatrix" instance --
## a list of functions for fetching and changing that matrix, and
## computing (or fetching) its inverse.
##
## Design note: there is no member function for _setting_ the inverse
## It is unlikely a client would use a "CacheMatrix" to store a known
## inverse, and allowing an inverse to be inserted is an open invitation
## to storing (and retrieving) an incorrect value.
##
## Arguments:
##   x = the matrix to cache (and invert)
## Value:
##   a list of functions:
##      setMatrix(x)   : set a new matrix (x) and clear the cached inverse
##      getMatrix()    : get the cached matrix
##      getInverse()   : get the inverse matrix (computing it if necessary)
##      resetInverse() : reset the inverse matrix
##
makeCacheMatrix <- function(x = matrix()) {
  invX <- NULL                # cached inverse
  # matrix setter
  setMatrix <- function(z) {
    x <<- z                   # set the matrix
    invX <<- NULL             # reset the inverse
  }
  # matrix getter
  getMatrix <- function() {
    x                         # return the matrix
  }
  # inverse getter
  getInverse <- function(...) {
    if (all(is.na(x))) {
      return(NULL)            # no matrix implies no inverse 
    } else if (is.null(invX)) {
      message("... computing inverse ...")
      invX <<- solve(x, ...)  # need to compute the inverse
    }
    invX                      # return the inverse
  }
  # clear the inverse matrix (but leave x intact) -- used if the inverse
  # needs to be recomputed with a different tolerance
  resetInverse <- function() {
    invX <<- NULL
  }
  # return a list of the functions (with x and invX embedded)
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       getInverse = getInverse, resetInverse = resetInverse)
}

##
## cacheSolve finds the inverse of a matrix stored as a "CacheMatrix".
## If the inverse has already been computed, it is fetched from cache
## and returned; otherwise it is computed (and cached).
##
## Arguments:
##   x         = a list produced by makeCacheMatrix())
##   recompute = if true, scrap a cached inverse (if any) and recompute it
##   ...       = optional arguments to the matrix inversion function
##               (ignored if the inverse is found in cache memory).
##               See base::solve for meaningful optional arguments.
## Value:
##   the inverse of the matrix stored in x
##
cacheSolve <- function(x, recompute = FALSE, ...) {
  if (recompute) {
    x$resetInverse()
  }
  x$getInverse(...)
}
