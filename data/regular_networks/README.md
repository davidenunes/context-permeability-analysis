Context Permeability - Regular Networks
=======================================

# Summary #
The objective of this experiment is to determine the percentage of times, consensus is 
achieved with the context permeability model. We use a _**kregular**_ network model for 
all the social networks with a parameter _**k**_ homogenous in all the networks as well. 
The parameters we vary are the _number of networks_ and the _k_. In this experiment, 
_100_ agents interact until consensus is achieved or _2000_ simulation steps passed. 

### Parameters ###
* **network model** : K-Regular Network, in which all the nodes have _2k_ connections. (for k = 50 and 100 nodes, the network is fully connected);

* **k** : varied from _[1,5]_ and from _10_ to _50_ in intervals of _10_;

* **number of networks** : varied from _[1,5]_;

### Target of Observation ###
We want to observe the _percentage of times consensus was achieved_ with each set-up and also 
some network properties such as _clustering coefficient_ and _average path length_.
