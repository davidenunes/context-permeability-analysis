#range normalization function
normal.range <- function(x){
  (x-min(x))/(max(x)-min(x))
}

#*******************************************************************************
# Reads a parameter file (normaly each experiment has an associated
# Parameter Space File) with all the parameter values used.
#
#@param param_file_name: a string with the filename / path to the parameter file
#*******************************************************************************
read.exp.parameters <- function(param_file_name){
  library(data.table)
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
  library(stargazer)

  stargazer(params, title="Simulation Parameters", type="html", align=FALSE, summary = FALSE)
}


plot.exp.box <- function(data,
                         x_factor,
                         data_y,
                         fill_factor,
                         fill_label,
                         y_label,
                         x_label){
  
  library(ggplot2)
  
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
  r_op_data <- melt(opinion_data, id=(c("step")), measure.vars=(c("num-opinion-0","num-opinion-1")))
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
  library(data.table)
  opinion_data <- fread(filename)
  opinion_data <- as.data.frame(opinion_data)
  opinion_data<- opinion_data[,-c(ncol(opinion_data))]
  return(opinion_data)
}

plot_persp_span <- function(x,y,data_matrix,breaks,xlab,ylab,zlab, pallete="YlOrBr"){ 
    z<- data_matrix
    nrz <- nrow(z)
    ncz <- ncol(z)
    
    jet.colors <- colorRampPalette(brewer.pal(n=9, name=pallete))

    nbcol <- 9
    color <- jet.colors(nbcol)
    
   
    
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
create_contour_span <- function(span,brks,guide_title,xlab,ylab){
  library(ggplot2)
  library(RColorBrewer)
  library(directlabels)
  
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
  v <- v + labs(x=xlab, y=ylab)
  
  plot <- direct.label(v,method="top.pieces")
  return(plot)
}

create_tolerance_contour_span <- function(span,brks,guide_title,xlab,ylab){
  library(ggplot2)
  library(RColorBrewer)
  library(directlabels)
  
  jet.colors <- colorRampPalette(brewer.pal(n=9, name="YlOrBr"))  
  nbcol <- 9
  color <- jet.colors(nbcol)
  
  #I have to do this otherwise ggplot cant find the variables in this function environmnet
  .e <- environment()
  
  v <- ggplot(span, aes(x=span$"ct0", y=span$"ct1", z=span$"value"), environment=.e)
  v <- v + geom_tile(aes(fill = span$"value"),breaks=brks) + scale_fill_gradientn(colours=color)
  v <- v + guides(fill = guide_colorbar(barwidth = 0.5, barheight = 20, title = guide_title))
  v <- v + stat_contour(aes(colour = ..level..), breaks=brks) + scale_colour_gradient(low = "black", high ="black")
  v <- v + labs(x=xlab, y=ylab)
  
  plot <- direct.label(v,method="top.pieces")
  return(plot)
}



tabular.cast_df <- function(xx,...)
{
  # a bunch of assumptions that must be met for this function to work:
  if(!require(reshape)) stop("The {reshape} package must be installed for this function to work")
  if(!require(tables)) stop("The {tables} package must be installed for this function to work")
  if(! any(class(xx) == "cast_df")) stop("This function only works for cast_df objects")
  # xx is a casted object
  
  m_xx <- melt(xx)
  rdimnames_xx <- attr(xx, "rdimnames")
  if(length(rdimnames_xx)>2) stop("This function only works for 2D tables")
  
  ROWS <- colnames(rdimnames_xx[[1]])
  COLUMNS <- colnames(rdimnames_xx[[2]])
  colnames_m_xx <- colnames(m_xx)
  
  # This is for cases when one of the equations has "(all)" in them due to something like cast(DATA, x ~.)
  if(all(ROWS == "value")) ROWS <- 1
  if(all(COLUMNS == "value")) COLUMNS <- 1
  
  if(any(colnames_m_xx == "value.1")) {	# then we are supposed to have a "(all)" case (e.g: cast(DATA, .~x)  ) 
    # m_xx <- m_xx[, -c(which(colnames_m_xx == "value")[-1])] # then remove the column with no value but "(all)"	# This would only work for cast(DATA, x~.) and not for cast(DATA, .~x)
    m_xx[,"value"]   <- m_xx[,"value.1"]
    column_where_all_is <- which(colnames_m_xx  == "value.1")
    m_xx <- m_xx[, -column_where_all_is] # then remove the column with no value but "(all)"
    colnames_m_xx <- colnames(m_xx)
  }
  if(sum(colnames_m_xx == "value") > 1 ) {	# then we are supposed to have a "(all)" case (e.g: cast(DATA, x~.)  ) 
    # m_xx <- m_xx[, -c(which(colnames_m_xx == "value")[-1])] # then remove the column with no value but "(all)"	# This would only work for cast(DATA, x~.) and not for cast(DATA, .~x)
    column_where_all_is <- which(m_xx[1,] == "(all)")
    m_xx <- m_xx[, -column_where_all_is] # then remove the column with no value but "(all)"
    colnames_m_xx <- colnames(m_xx)
  }
  
  LEFT <- paste(ROWS , collapse="*")
  RIGHT <- paste(COLUMNS , collapse="*")
  
  # turn all ROWS/COLUMNS variables into factors - so to make sure that the tabular will work on them as we expect
  column_to_turn_into_factor <- intersect(c(ROWS, COLUMNS), colnames_m_xx)	# this removes the "1"s in case of cast(DATA, x~.) 
  for(i in column_to_turn_into_factor) m_xx[,i] <- factor(m_xx[,i])
  
  # Further motivation for the above two lines have been given by Duncan (on 11.12.11): 
  # The problem here is that tabular() needs to figure out what you want to do with each variable.  value and month are both numeric, so it can't tell which one you want as an analysis variable.  temp2 is a character variable; those are also treated as possible analysis variables, but perhaps they should be treated like factors instead.  (But then there would need to be syntax to say "don't treat this character as a factor".)
  # So another way to get what you want would be to change the table spec to
  # tabular(value*v*factor(month)*factor(temp2) ~ variable2*result_variable, data = m_xx)
  # but this changes the headings too; so maybe I should have a function Factor that does what factor() does without changing the heading. Here's a quick definition:
  # Factor <- function( x ) substitute(Heading(xname)*x, list(xname = as.name(substitute(x)), x = factor(x)))
  # tabular(value*v*Factor(month)*Factor(temp2)~variable2*result_variable, data = melt(xx), suppress=2)
  
  v <- function(x) x[1L]
  txt <- paste("tabular(value*v*", LEFT , "~" ,RIGHT ,", data = m_xx, suppressLabels  = 2,...)", sep = "")
  # suppressLabels is in order to remove the value and the v labels (which are added so to make sure the information inside the table is presented)	
  eval(parse(text = txt ))
}

