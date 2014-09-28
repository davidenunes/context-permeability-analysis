Context Permeability Analysis
=============================

To reproduce any experiment from the `data` folder 
you just need to run the model written in java with the parameter `-f`
as follows: 

Run the jar with the -f model inside the folder with the configuration file. 
This runs the simulation model and generates the data files inside that folder.

```shell

java -jar <pathto>/cp-model.jar -f <cfgfile>.cfg

```

This produces some files which are then used by `knitr` to create the 
html reports. The analysis code was written in R Markdown `.Rmd`. The 
code for these reports might require a slight adjustement since some 
file names include a timestamp that identifies when they were created. 
