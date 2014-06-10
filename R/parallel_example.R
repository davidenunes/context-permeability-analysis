library(parallel)

## Number of workers (R processes) to use:
numWorkers <- 8



measure_parallel <- function(cfg,r){
  data <- network_data
  network <- read.big.network(data,cfg,r)
  properties <- measure.network(network)
  properties <- cbind(r,properties)
  return(properties)
}

props <- NULL
num_networks <- c(1,2,3,4,5)
k_values <- seq(10,50,10) 
runs <- sort(unique(network_data[,"run"]))

require(BBmisc)#for progress bar
pb <- makeProgressBar(min=0, max=(length(num_networks)*length(k_values)), label="network analysis")

#pre-alocate a matrix for the network properties
props <- matrix(nrow=(length(num_networks)*length(k_values)*length(runs))-1, ncol=7)
i <- 0


for(n in num_networks){
  #reads the properties for all the k-regular networks with the given k and num_networks 
  for(k in k_values){
    cfg <- params[params$"num.network" == n & params$"network.0.k" == k,][[1]]
    
    properties <- mcmapply(cfg=cfg, r=runs, measure_parallel, mc.cores = numWorkers)
    properties <- t(properties)
    
      
    props[(i+1):(i+100),] <- cbind(k,n,properties)
    i <- i + 100  
    #some progress feddback
    pb$inc(1,paste("reading n=",n,"k=",k,"\n"))
    gc()
  } 
}
pb$kill()

colnames(props)<-c("k","networks","run","nodes","edges","clustering-coefficient","average-path-length")

#clean up
rm(pb)
gc()

