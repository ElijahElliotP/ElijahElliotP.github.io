# R
# Radian to degree conversion
r2d <- function(rad) {(rad * 180) / (pi)}
d2r <- function(deg) {(deg * pi) / (180)}

# Population variance and standard deviation
pop.var <- function(x) {var(x) * (length(x)-1) / length(x)}
pop.sd <- function(x) {sqrt(pop.var(x))}

ls.funs <-
function(env=sys.frame(-1))unlist(lapply(ls(env=env),function(x)if
(is.function(get(x)))x))

ls.vars <-
function(env=sys.frame(-1))unlist(lapply(ls(env=env),function(x)if
(!is.function(get(x)))x))

########################################################################
smallType <- c('CONGALTON','STEHMAN','SE')
bigType <- c('area','producers','users')
bigType_alt <- c('producers','users')
columns <- c('congProd','congUser','stehArea','stehProd','stehUser','stehPrSe','stehUsSe')
classes <- c("1","2","3")

getFromFiles <- function() {
    myArray  <- matrix()
    for (i in classes) {
        for (j in smallType) {
            if (j == 'STEHMAN') {
                for (k in bigType) {
                    tmp.df <- read.csv(
                        file = paste("event.",i,'.',k,'.',j,'.csv',sep=''),
                        header = T,
                        row.names = 1)
                    n <- tmp.df[1,1]
                    print(c("n",n))
                    myArray <- rbind(myArray,n)
                }
            } else {
                for (k in bigType_alt) {
                    tmp.df <- read.csv(
                        file = paste("event.",i,'.',k,'.',j,'.csv',sep=''),
                        header = T,
                        row.names = 1)
                    n <- tmp.df[1,1]
                    print(c("n",n))
                    myArray <- rbind(myArray,n)
                }
            }
        }
    }
    myArray <- myArray[-1]
    myArray <- data.frame(matrix(myArray,nrow = length(classes),ncol = length(columns),byrow = T))
    row.names(myArray) <- classes
    colnames(myArray) <- columns
    myArray
    write.csv(myArray,file = paste('aFullSummary','.csv',sep=''))
    return(myArray)
}
