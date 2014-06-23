EXPERIMENTS WITH CONTEXT PERMEABILITY AND SCALE-FREE NETWORKS
=============================================================

In these experiments, we use the **context permeability** model, the one in which the agents
interact in multiple networks one pair at a time. We explore the percentage of convergences
to total consensus and the number of encounters necessary to achieve this consensus. 

We explore scale-free networks constructed according to the Barabasi-Albert model. Networks 
grow using a mechanism of prefferential attachment. We have a parameter _**d**_ which dictates the 
number of links added using prefferential attachment each time a new node is added to the network. 

This parameter increases the network connectivity. For _**d = 1**_ the network is a forest, all the subgraphs from the most connected node are tree graphs. 

To create the scale-free networks we used the BAModel from the bhave network library which is embedded 
in the context segregation model.

see [context-permeability model source](https://github.com/social-simulation/context-permeability)
