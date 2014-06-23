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

plot_persp_span <- function(x,y,data_matrix,breaks,xlab,ylab,zlab, pallete="YlOrBr"){ 
    z<- data_matrix
    nrz <- nrow(z)
    ncz <- ncol(z)
    
    jet.colors <- colorRampPalette(brewer.pal(n=9, name=pallete))

    nbcol <- 9
    color <- jet.colors(nbcol)
    
    #range normalization function
    normal.range <- function(x){
      (x-min(x))/(max(x)-min(x))
    }
    
    #normalized breaks
    normbreaks <- normal.range(unique(breaks))

    zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
    # Recode facet z-values into color indices
    facetcol <- cut(zfacet, breaks=normbreaks*max(zfacet))
  
  
    persp_plot <- persp(x, y, z,expand=1, ylab=ylab, xlab=xlab, zlab=zlab,ticktype="detailed", col=color[facetcol], phi=45, theta=135,  cex=2)
    return(persp_plot)
}



#'Takes the aggregated data for the number of encounters containing 3 columns ("cs0", "cs1", and "value")
#'and returns a contour plot. The contour lines and filling are based on the breaks which are also supplied.
#'The cusom breaks are the values for which the user desires a contour line. 
#'
#'@param data a data frame with 3 columns:
#'cs0 - the switching probability for network 0
#'cs1 - the switching probability for network 1
#'value - the value we are plotting
#'
#'@param brks the breaks for wich we want the contour lines
#'
#'@param the title for the colourband guide which goes with the filling 
#'(the colour filling is also constructed based on the breaks)
#'
#'
create_contour_span <- function(span,brks,guide_title){
  require(ggplot2)
  require(RColorBrewer)
  require(directlabels)
  
  jet.colors <- colorRampPalette(brewer.pal(n=9, name="YlOrBr"))  
  nbcol <- 9
  color <- jet.colors(nbcol)
  
  #I have to do this otherwise ggplot cant find the variables in this function environmnet
  .e <- environment()
  
  v <- ggplot(span, aes(x=span$"cs0", y=span$"cs1", z=span$"value"), environment=.e)
  #v <- v + geom_tile(aes(fill = span$"value"),breaks=brks) + scale_fill_gradientn(colours=brewer.pal(n=10, name="Paired"), breaks=brks)
  v <- v + geom_tile(aes(fill = span$"value"),breaks=brks) + scale_fill_gradientn(colours=color)
  v <- v + guides(fill = guide_colorbar(barwidth = 0.5, barheight = 20, title = guide_title))
  v <- v + stat_contour(aes(colour = ..level..), breaks=brks) + scale_colour_gradient(low = "black", high ="black")
  v <- v + labs(x="Switching probability from network 1", y="Switching probability from network 2")
  
  plot <- direct.label(v,method="top.pieces")
  return(plot)
}
