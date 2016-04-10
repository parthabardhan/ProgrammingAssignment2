## makeCacheMatrix creates the "special matrix"
## it is actually a list consists of 4 functions
##        1. set -- to set a new matrix. It also clears the calculated inverse if any
##        2. get -- returns the matrix
##        3. setinv -- set the inverse supplied.
##        4. getinv -- get the cached inv.
##
##cacheSolve calculates the inverse of the "special matrix" given by makeCacheMatrix
##           but it does not calculate if it's aleady calculated. in this case it simply
##           returns the cached inverse
## 

## makeCacheMatrix takes a matrix and returns a list of 4 functions
## so that for that matrix the inverse can be cached.

makeCacheMatrix <- function( x = matrix() ) {
     inv <- NULL
     set <- function( y ){
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinv <- function( inverse ) inv <<- inverse
     getinv <- function() inv
     
     list( set = set,
           get = get,
           setinv = setinv,
           getinv = getinv )
}


## cachesolve is the cashed version of solve, where it will take a "special matrix" 
## as described above and returns inverse of it

cacheSolve <- function( x, ... ) {
     ## Return a matrix that is the inverse of 'x'
     inv <- x$getinv()
     if( !is.null( inv ) ){
          # Ok, inverse is already calculated, so just return it
          # This message is just to prove that it went thru this path
          message( "Inverse already Calculated: returning from cache" )
          return( inv )
     }
     # no inverse, so calculate and cache it
     inv <- solve( x$get() )
     x$setinv( inv )
     inv
}
