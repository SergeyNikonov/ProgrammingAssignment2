#Code for  programming assignment 2 (rprog-004). Code based on example 
#makeVector and cachemean functions

#Creates a matrix object with a list of 4 functions 
makeCacheMatrix <- function(m = matrix()) {
  solved <- NULL                          
  set <- function(a) {#Set "method" globally gets data and nullifies inverse
    m <<- a
    solved <<- NULL
  }
  get <- function() m #Get "method" returns matrix contents
  setsolved <- function(solve) solved <<- solve  #Globally sets inverse
  getsolved <- function() solved                #Shows inverse
  list(set = set, get = get,
       setsolved = setsolved,
       getsolved = getsolved)
}

#A fuction to cache or retrieve cached inverse matrix

cacheSolve <- function(m, ...) {
  solved <- m$getsolved()
  if(!is.null(solved)) {        #Test cache existence and retrieving it
    message("retrieving cache")
    return(solved)
  }
  data <- m$get()
  solved <- solve(data)         #Creates new inverse matrix
  m$setsolved(solved)           
  solved
}

