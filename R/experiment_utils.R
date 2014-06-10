#*******************************************************************************
# Reads a parameter file (normaly each experiment has an associated
# Parameter Space File) with all the parameter values used.
#
#@param param_file_name: a string with the filename / path to the parameter file
#*******************************************************************************
read.exp.parameters <- function(param_file_name){
  params <- read.csv(param_file_name, header=TRUE, sep=";")
  #remove empty column at the end
  params<- params[,-ncol(params)]
  return(params)
}


#*******************************************************************************
# Takes a a previously read parameter table and 
# prints it using the stargazer library. 
# 
# This is useful mainly to generate reports. 
# Note: for printing in knitr use "results='asis',message=FALSE" 
# in the chunk options
#
# @param parameters: a previously loaded parameter table
#*******************************************************************************
print.exp.parameters <- function(parameters){
  require(stargazer)

  stargazer(params, title="Simulation Parameters", type="html", align=FALSE, summary = FALSE)
}

require(ggplot2)
plot.exp.box <- function(data,
                         x_factor,
                         data_y,
                         fill_factor,
                         fill_label,
                         y_label,
                         x_label){
  .e <- environment()
  plot <- ggplot(aes(y=data_y,x=x_factor,fill=fill_factor), data=data, environment=.e)  
  plot <- plot + geom_boxplot()
  plot <- plot + labs(fill=fill_label, y=y_label, x=x_label)
  return(plot) 
}

#***********************************************************************
# RETURNS A LINE PLOT WITH THE OPINION PROGRESSION FOR A PARTICULAR 
# CONFIGURATION / RUN
#
#***********************************************************************
plot_opinion <- function(opinion_data){
  op_run <- as.data.frame(opinion_data)
  r_op_data <- melt(op_run, id=(c("step")), measure.vars=(c("num.opinion.0","num.opinion.1")))
  names(r_op_data)<- c("step", "opinion", "value")
  
  .e <- environment()
  
  plot <- ggplot(data = r_op_data, aes(x = r_op_data$step, y = r_op_data$value , color=r_op_data$opinion,), environment=.e)
  plot <- plot + geom_line() + geom_point(size=0)
  plot <- plot + labs(x = "Simulation Step", y = "Number of Agents")
  #plot <- scale_fill_discrete(name="Opinion",
  #                        breaks=c("num.opinion.0", "num.opinion.1"),
  #                       labels=c("Opinion Value 1", "Opinion Value 2"))
  plot <- plot + labs(title = "Opinions")
  plot <- plot + theme_bw()
  plot <- plot + theme(legend.title=element_blank())
  return(plot)
}

read.opinion.progress <- function(filename){
  opinion_data <- read.csv(filename, sep=";")
  opinion_data<- opinion_data[,-ncol(opinion_data)]
  return(opinion_data)
}

