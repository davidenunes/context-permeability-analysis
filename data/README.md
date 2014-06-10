Context Permeability Experiments
=================================

# Model Description #
In this model agents interact in multiple networks simultaneously. Agents have to choose between
two possible opinion values. This can be seen as a binary opinion dynamics model. The agents interact with 
each other and update their opinion values in an asynchronous fashion. The simulation stops if the required
consensus ratio is achieved (_in this case 1.0_) or if _2000_ steps are executed without reaching consensus.

# Model Availability #


* **GitHub URL repository**: [https://github.com/social-simulation/context-permeability](https://github.com/social-simulation/context-permeability)


TODO: 
* add information about the version of the code (create a tag with the release) this is to be done after the model is verified
* add a description of the model in the README.md file 
* add information about how to use the source code and how to build the model
* add information about how to run the model in the README.md file 




# Experiment 1 - Model Validation #

* **folder**: model_validation

## Summary ##
The idea of this experiment is to validate the simulation model and produce data using 
a basic set of configuration namely using different network models and a different number 
of network layers. 