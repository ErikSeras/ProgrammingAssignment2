################################################################################
## A pair of functions that cache the inverse of a matrix
## Un par de funciones que almacenan en caché el inverso de una matriz


## Function that creates a special matrix object that can cache its inverse

## Función que crea un objeto matriz especial que puede almacenar en caché
## su inverso

makeCacheMatrix <- function(m = matrix()) {
        ## Initialize the inverse property
        ## Inicializa la propiedad inversa
        i <- NULL
        
        ## Method to set the matrix
        ## Método para establecer la matriz 
        
        set <- function(matrix){
                m <<- matrix
                i <<- NULL
        }
        ## Method to get the matrix
        ## Método para obtener la matriz
        get <- function(){
                ## Return the matrix
                ## Retornar la matriz
                m
        }
        
        ## Method to set the inverse of the matrix
        ## Método para establecer la inversa de la matriz
        setInverse <- function(inverse){
                i <<- inverse
        }
        
        ## Method to get the inverse of the matrix
        ## Método para obtener la inversa de la matriz
        getInverse <- function(){
                ## Return the inverse property
                ## Retornar la propiedad inversa
                i
        }
        
        ## Return a list of the methods
        ## Retorna una lista de los métodos
        list(
                set = set,
                get = get,
                setInverse = setInverse,
                getInverse = getInverse
        )
}

################################################################################

## Compute the inverse of the special array returned by "makeCacheMatrix"
## above.
## If the inverse has already been calculated (and the array has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

## Calcular el inverso de la matriz especial devuelta por "makeCacheMatrix"
## arriba.
## Si el inverso ya se ha calculado (y la matriz no ha cambiado),
## entonces "cachesolve" debería recuperar el inverso del caché.
cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        ## Retornar una matriz que is el inverso de 'x'
        m <- x$getInverse()
        
        ## Just return the inverse if it's already set
        ## Solo devolver el inverso si ya está configurado
        if( !is.null(m) ) {
                message("getting cached data")
                return(m)
        }
        
        ## Get the matrix from object
        ## Obtener la matriz del objeto
        data <- x$get()
        
        ## Calculate the inverse using matrix multiplication
        ## Calcular el invero usando la multiplicación de matrices
        m <- solve(data, ...)
        
        ## Another way to compute the above
        ## The values of the resulting matrix are in scientific notation
        ## Otra forma de calcular lo anterior
        ## Los valores de la matriz resultante están en notación científica.
        ## m <- solve(data) %*% data
        
        ## Set the inverse to the object
        ## Establecer el inverso al objeto
        x$setInverse(m)
        
        ## Return the matrix
        ## Devolver la matriz
        m
}

################################################################################

# Testing
# Prueba

normal_matrix <- matrix(c(1,7,5,5,5,5,1,8,9,1,4,7,5,9,1,4), 4, 4)
normal_matrix

inverse_matrix<- makeCacheMatrix(normal_matrix)
cacheSolve(inverse_matrix)

