## R Programming Assignment2: write an R function is able to cache potentially time-consuming computations.
## Program Purpose: Caching the inverse of a Matrix

## Matrix inversion is a costly computation. There are many benefits to using caching the invers of a 
## matrix rather than compute it repeately.

## The following pair of functions are used to cache the invers of a square invertible matrix

## This first function MakeCacheMatrix create a special "matrix" object that can cache its inverse 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()){
         m <- NULL
         set <- function(y){
               x <<- y 
               m <<- NULL
         }
         get <- function() x
         setinverse <- function(inversematrix) m <<- inversematrix
         getinverse <- function() m 
         list(set = set, get = get,
              setinverse = setinverse, 
              getinverse = getinverse)
}

## This second function calculates the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the matrix has already been calculated(and the matrix has not changes), then cacheSolve should 
## retrive the inverse from the cache, If so, it gets the value from the cache and skip the calculation,
## Otherwise, it calculate the inverse of the matrix and set the value of the inverse of the matrix in
## the cache via the getinverse function

cacheSolve <- function(x = matrix()){
        ## Return a matrix that is the inverse of 'x'
         m <- x$getinverse()
         if(!is.null(m)){
            message("getting cache data")
            return()
         }
         
        ## use Solve() function to get the inverse of a matrix         
         data <- x$get()
         m <- Solve(x)
         x$setinverse(m)
         m
}