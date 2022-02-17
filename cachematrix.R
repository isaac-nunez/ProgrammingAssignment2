## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#The function  includes an optional argument to supply a matrix that may be invertible.
#If no matrix is supplied then it will automatically create an invertible (squared) matrix populated by random numbers obtained from a 
#uniform distribution. The dimensions of the created matrix may be modified with several arguments
#supplied within the function (x for the length of a side, y for the lower limit of the uniform
#distribution, and z for the upper limit of the uniform distribution).
#If the cache argument is TRUE, then it will automatically calculate and store the inverse of the
#matrix created. If it is FALSE, either the supplied matrix or the created matrix will be 
#returned as is.

makeCacheMatrix <- function(mat=NULL, x=4, y=0, z=100, cache=T){
  if(!is.null(mat)){
    if(dim(mat)[1]==dim(mat)[2]){
      if(cache==T){
        mat<<-mat
        inverse <<- solve(mat)
        inverse  
        print(inverse)
        print("The supplied matrix was inverted and stored in caché")
      }else{
        inverse<<-NULL
        mat <<- mat
        print(mat)
        print("The supplied matrix was not inverted")
      }
    }else{
      print("The supplied matrix is not invertable")
    }
  }else{
    if(cache==T){
      mat <- matrix(round(runif(n=x^2, min=y, max=z)), nrow=x, ncol=x) 
      inverse <<- solve(mat)
      inverse  
      print(inverse)
      print("The created matrix was inverted and stored in caché")
    }else{
      mat <- matrix(round(runif(n=x^2, min=y, max=z)), nrow=x, ncol=x) 
      inverse<<-NULL
      print(mat)
      print("The created matrix was not inverted")
    }
  }
}

#Several examples to show it works
makeCacheMatrix()
makeCacheMatrix(mat=matrix(round(runif(25, 0,200)), 5, 5))
makeCacheMatrix(mat=matrix(round(runif(25, 0,200)), 5, 5), cache =F)
makeCacheMatrix(matrix(round(runif(30, 0,200)),3, 10))
makeCacheMatrix(cache=F)
makeCacheMatrix(x=10, y=5, z=10, cache=T)
makeCacheMatrix(x=10, y=5, z=10, cache=F)


#This function does not take arguments. It automatically detects wether the inverse of the
#matrix of interest exists, and if it does it determines if it is non null. If that is the case,
#then it retreives the value from the cache and prints it.
#If the inverse does not exist, it calculates it from the matrix of interest.
#This function requires the function makeCacheMatrix to be called at least once before.
cacheSolve <- function(){
  if(exists("inverse")){
    if(!is.null(inverse)){
      print(inverse)
      print("Inverse of matrix was found in caché")
    }else{
      mat_s <- solve(mat)
      print(mat_s)
      print("Inverse of matrix was NOT found in caché and had to be calculated")
    }
  }else{
    mat_s <- solve(mat)
    print(mat_s)
    print("Inverse of matrix was NOT found in caché and had to be calculated")
  }
}

#Several examples to show it works
makeCacheMatrix()
cacheSolve()

makeCacheMatrix(mat=matrix(round(runif(25, 0,200)), 5, 5))
cacheSolve()

makeCacheMatrix(mat=matrix(round(runif(25, 0,200)), 5, 5), cache =F)
cacheSolve()

makeCacheMatrix(matrix(round(runif(30, 0,200)),3, 10))
cacheSolve()

makeCacheMatrix(cache=F)
cacheSolve()

makeCacheMatrix(x=10, y=5, z=10, cache=T)
cacheSolve()

makeCacheMatrix(x=10, y=5, z=10, cache=F)
cacheSolve()

