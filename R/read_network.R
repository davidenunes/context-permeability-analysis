library(igraph)


#********************************************************************
# READS NETWORK DATA AND RETURNS AN ARRAY OF IGRAPH INSTANCES
#
# network csv data
#
# returns a list with all the igraph objects, you can access it with
# the  [[]] operator
#********************************************************************
read.network <- function(network_data, layer){
  
  num_networks <- max(network_data[ ,network_data$"run"])
  
  networkv <- vector("list",num_networks)
  
  #create the networks
  for(r in 1:num_networks){
    graph <- read_network(network_data,r,layer)
    networkv[[(r)]] <- graph
  }
  return(networkv)
}

#******************************************************
# In the context permeability simulations networks are 
# written to a file at the end of each simulation run
# this function READS A NETWORK FOR A PARTICULAR RUN
#
# @param networkData data for the network with a field 
# "run for each network"
# @param run the run number from which we want the 
# network.
#
#
# this assunes the networkData has columns for 
# run, node1, node2 where run is the current run the 
# network belongs to, node1 and node2 are ids of nodes
# representing an undirected edge.
#
#*******************************************************
read.network <- function(network_data, run, layer){
  networks <- as.data.frame(network_data)
  
  #create the networks
  network <- networks[networks$"run" == run & networks$"layer" == layer, ]
  
  #no network found for that layer
  if(nrow(network) == 0)
    stop(paste("No network found for layer",toString(layer)))
  
  #only the edge list
  network <- network[,c("node1","node2")] 
  #load the edge list into an igraph object
  graph <- graph.data.frame(d=network, directed=FALSE, vertices=NULL)
    
  return(graph)
}

#********************************************************
# READS A SINGLE NETWORK, THIS REQUIRES THE DATA TO BE
# FILTERED TO INCLUDE THE DATA OF THE NETWORKS FOR A SINGLE
# RUN
#
# THE LAYER IS THE NETWORK WE WANT TO READ
#********************************************************
read.single.network <- function(data, layer){
  networks <- as.data.frame(data)
  network <- networks[networks$"layer" == layer, ]
  if(nrow(network) == 0)
    stop(paste("No network found for layer",toString(layer)))
  network <- network[,c("node1","node2")] 
  
  graph <- graph.data.frame(d=network, directed=FALSE, vertices=NULL)
  return(graph)
}

#*******************************************************
#READS A NETWORK FOR A PARTICULAR CONFIGURATION AND RUN
#
#RETURNS A GRAPH WITH THE COMPOSED NETWORK
#
# The function first filters the network data by configuration 
# and run number for that configuration
#*******************************************************
read.composite.network <- function(network_data){
  layers <- unique(network_data$"layer")
  network <- NULL
  for(layer in layers){
    current_network <- read.single.network(data=network_data, layer=layer)
    if(is.null(network)) network <- current_network else network <- network + current_network
  }
  return(network)
}

#***********************************************************
# GIVEN A NETWORK, read its properties
# 
#
#***********************************************************
read.composite.properties <- function(network){
  num_nodes <- c(length(V(network)))
  num_edges <- c(length(E(network)))
  clustering <- c(transitivity(network, type = c("globalundirected")))
  avplength <- c(average.path.length(network, directed = FALSE))
  
  props <- cbind(num_nodes, num_edges, clustering, avplength)
  
  
  props <- as.data.frame(props)
  colnames(props) <- c("nodes", "edges", "cc", "apl")
  
  return(props)
}


#******************************************************
# READ THE PROPERTIES OF REGULAR NETWORKS
# CREATE A DATA FRAME INCLUDING THE K Values and the 
# NUMBER OF NETWORKS FOR EACH PROPERTY
#
# parameters:
#     network_data - the network data for the regular networks 
#     parameter_data - a table with the simulation parameters
#     k_values - a vector with the k values (this assumes the data has these values)
#     num_network_values - the number of networks merged to create the final network 
#                          which was in turn analysed
#
#****************************************************** 
read.regular.props <- function(network_data, params, k_values, num_network_values){
  network_props <- NULL
  
  params <- params[,c("cfg.id","num.networks", "network.0.k")]
  network_data <- network_data[,-ncol(network_data)]
  
  networks <- merge(network_data, params, by="cfg.id")
  
  for(numnet in num_network_values){ 
    for(k in k_values){
      #network layers filtered by k and number of networks
      f_network <- networks[networks$"num.networks" == numnet & networks$"network.0.k" == k,]
    
      #read the properties of all the composite networks for the curren k and numnet
      current_network_props <- read.network.props(network_data=f_network) 
      
      k_prop <- rep(k,nrow(current_network_props))
      num_networks_prop <- rep(numnet, nrow(current_network_props))
      current_network_props <- cbind(current_network_props, k_prop, num_networks_prop)
      
      #concat the results in a single table
      if(is.null(network_props)) network_props <- current_network_props  else network_props <- rbind(network_props, current_network_props)
    }
  }
  
  network_props <- as.data.frame(network_props)
  return(network_props)
}

#this works with bigmemory matrixes
#the parameter data is a normal dataframe
read.knetwork.properties <-function(network_matrix,network_params,k_set,num.networks_set){
  network_props<-NULL
  #for each value of n and k
  for(n in num.networks_set){ 
    for(k in k_set){
      #1. find the configuration for (n,k)
      cfg <- network_params[network_params$"num.networks" == n & network_params$"network.0.k"==k,][[1]]
      
      #you know which configuration is equivalent to (n,k), filter the dataset based on that
      #cols 3 is cfg.id
      cfg_id_col <- which(colnames(network_matrix)=="cfg.id")
      sub_matrix_rows <- mwhich(x=network_matrix, cols=cfg_id_col, vals=as.list(cfg),comps=list('eq'))
      
      run_col <- which(colnames(network_matrix)=="run")
      run_set <- sort(unique(network_matrix[,run_col]))
      
      #read the properties for all the runs
      current_network_props <- read.knetwork.run.properties(network_matrix=network_matrix,rows=sub_matrix_rows,run_set=run_set) 
      
      k_prop <- rep(k,nrow(current_network_props))
      num_networks_prop <- rep(numnet, nrow(current_network_props))
      current_network_props <- cbind(current_network_props, k_prop, num_networks_prop)
      
      #concat the results in a single table
      if(is.null(network_props)) network_props <- current_network_props  else network_props <- rbind(network_props, current_network_props)
    }
  }
  
  network_props <- as.data.frame(network_props)
  return(network_props)
}

#to be used with bigmemory
#reads all the properties for the networks in all the runs
read.knetwork.run.properties <- function(network_matrix,rows,run_set){
  num_nodes <- c()
  num_edges <- c()
  clustering <- c()
  avplength <- c()
  
  #get properties for all the runs
  for(r in run_set){
    #filter by run, get all the networks for run r
    r_networks_rows <- mwhich(x=network_matrix, cols=run_col, vals=as.list(r),comps=list('eq'))
    
    current_networks <- network_matrix[r_networks_rows,]
    edge_columns <- match(c("node1","node2"),colnames(network_matrix))
    current_networks_edges <- network_matrix[,edge_columns]
    
    #load the edge list into an igraph object
    graph <- graph.data.frame(d=current_networks_edges, directed=FALSE, vertices=NULL)
    
    num_nodes <- c(num_nodes, length(V(graph)))
    num_edges <- c(num_edges, length(E(graph)))
    clustering <- c(clustering, transitivity(graph, type = c("globalundirected")))
    avplength <- c(avplength, average.path.length(graph, directed = FALSE))

  }
  
  props <- cbind(run_set, num_nodes, num_edges, clustering, avplength)
  
  
  
  props <- as.data.frame(props)
  #cc - clustering coefficient
  #apl - average path length
  colnames(props) <- c("run", "nodes", "edges", "cc", "apl")
  
  return(props)
  
}




#****************************************************************
# Loads the networks (previously fildered by the given config id)
# and store the vectors of properties such as clustering coefficient
# average path length etc.
# returns a data.frame with the properties
# Note:   
# cc - clustering coefficient
# apl - average path length
#
#****************************************************************
read.network.props <- function(network_data){
  num_nodes <- c()
  num_edges <- c()
  clustering <- c()
  avplength <- c()
  
  
  num_runs <- max(network_data$run)
  for(r in 1:num_runs){ 
    max_layer <- max(network_data$layer)
    network <- NULL
    #merge layers
    for(l in 0:max_layer){
      currentNetwork <- read.network(network_data=network_data, run=r,layer=l)
      if(is.null(network)) network <- currentNetwork else network <- network + currentNetwork
    }
    #collect measures
    num_nodes <- c(num_nodes, length(V(network)))
    num_edges <- c(num_edges, length(E(network)))
    clustering <- c(clustering, transitivity(network, type = c("globalundirected")))
    avplength <- c(avplength, average.path.length(network, directed = FALSE))
    #TODO ADD EMBEDENESS (edges shared between each pair of networks)   
  }
  
  props <- cbind(1:num_runs, num_nodes, num_edges, clustering, avplength)
  

  
  props <- as.data.frame(props)
  #cc - clustering coefficient
  #apl - average path length
  colnames(props) <- c("run", "nodes", "edges", "cc", "apl")
  
  return(props)
}

plot.degree.distribution <- function(network, title){
  graph <- network
  
  distribution<- degree(graph, V(graph),mode = c("all"))
  
  plot <- hist(log(distribution+1), main = title, col = "#7D9C9F") 
  return(plot)
}
