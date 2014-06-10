EXPERIMENTS WITH CONTEXT PERMEABILITY AND SCALE-FREE NETWORKS + REGULAR NETWORKS
================================================================================

In these experiments, we use the **context permeability** model, the one in which the agents
interact in multiple networks one pair at a time. We explore the percentage of convergences
to total consensus and the number of encounters necessary to achieve this consensus. 

We explore scale-free networks constructed according to the Barabasi-Albert model. Networks 
grow using a mechanism of prefferential attachment. We have a parameter _**d**_ which dictates the 
number of links added using prefferential attachment each time a new node is added to the network. 

This parameter increases the network connectivity. For _**d = 1**_ the network is a forest, all the subgraphs from the most connected node are tree graphs. 

To create the scale-free networks we used the BAModel from the bhave network library which is embedded 
in the context segregation model.

In this experiment agents interacted in exactly 2 networks. 

* **First Network :** K-Regular
* **Second Network :** Barabasi-Albert Scale-free Network

Parameter sweep performed:

* **k values for k-regular network:** {1,2,3,4,5,10,20,30,40,50}
* **d number of links added each time for scale-free network:** {1,2,3,4,5}


We explored k values from 1 to 5 because in previous experiments 
using only regular networks the convergence rate was very low, we
want to observe what happens when you overlay scale-free networks.

_For other parameters see file **sfreg.cfg**_


see [context-permeability model source](https://github.com/social-simulation/context-permeability)
