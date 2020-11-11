
library(EntropyMeasures)
library(RandomFields)
library(spatstat)
library(rgl)
library(plot3D)
library(fields)
library(SDraw)
library(raster)
library(data.table)
library(ggplot2)
library(plotly)
library(lattice)
library(beepr)
spatstat.options(npixel=c(180, 180)) 

#dir: Informational assesment of log-gaussian cox processes v.1/Simulaciones v.2



##### SIMULACION DEL PROCESO DE POISSON UNIFORME 

lambda <- rep(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20, 25, 
                30, 35, 40, 45, 50, 60, 70, 80, 90, 100), each = 100)

X <- list()

for(i in 1:length(lambda)){
  
  X[[i]] <- rpoispp(lambda[[i]], win=owin(c(0,10),c(0,10)))
  
}

# save(X, file="sim_PPint.RData")

X_PPS <- X

#Distribuciones de los patrones de puntos y del campo de intensidad

PPDist <- function(pp, numq = 10){
  qc <- list()
  dist <- list()
  for(i in 1:length(pp)){
    qc[[i]] <- as.vector(quadratcount(pp[[i]], numq))
    dist[[i]] <- qc[[i]] / sum(qc[[i]]) 
  }
  
  return(dist)
}

RFDist <- function(pp){
  
  rastr <- list()
  dist <- list()
  
  for(i in 1:length(pp)){
    
    rastr[[i]] <- as.vector(aggregate(raster(density(pp[[i]])),
                                      fact = 18, FUN = mean))
    dist[[i]] <- rastr[[i]] / sum(rastr[[i]]) 
  }
  
  return(dist)
}


dist_pps <- PPDist(X_PPS)
rfdist_pps <- RFDist(X_PPS)



### SIMULACION DE PROCESO DE POISSON Y WN - PARA USAR CON ORDEN


lambda <- rep(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20, 25, 
                30, 35, 40, 45, 50, 60, 70, 80, 90, 100), each = 500)

X <- list()

for(i in 1:length(lambda)){
  
  X[[i]] <- rpoispp(lambda[[i]], win=owin(c(0,10),c(0,10)))
  
}

save(X, file="sim_PPint2.RData")


rm(X)

X <- list()

for(i in 1:500){
  X[[i]]<-rpoispp(rnoise(w=owin(c(0,10),c(0,10))))
}

save(X,file="sim_wn2.RData")


beep(sound = 8)

X <- list()

for(i in 1:100){
  X[[i]]<-rpoispp(rnoise(runif, owin(c(0,10),c(0,10))))
}



save(X,file="sim_wn.RData")




