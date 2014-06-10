# opinion_progress <- read.csv("~/Dropbox/PhD/experiments/Context Permeability/data/model_validation/ba-scale-free-network/opinion_progress.csv", sep=";")
# get one run 
# op_run <- opinion_progress[opinion_progress$run == 1,]
#
# install rcharts
#require(devtools)
#install_github('rCharts', 'ramnathv')

library(rCharts)
require(reshape2)

opinion_progress <- read.csv("~/Dropbox/PhD/experiments/Context Permeability/data/model_validation/ba-scale-free-network/opinion_progress.csv", sep=";")
op_run <- opinion_progress[opinion_progress$run == 1,]

#reshapes the data to be ploted
reshaped_op_data <- melt(op_run, id=(c("step")), measure.vars=(c("num.opinion.0","num.opinion.1")))
x1 <- xPlot(value ~ step, group = "variable", data = reshaped_op_data, type = "line-dotted")


opinion_progress <- read.csv("~/Dropbox/PhD/experiments/Context Permeability/data/model_validation/ba-scale-free-network/opinion_progress.csv", sep=";")
op_run <- opinion_progress[opinion_progress$run == 1,]

#reshapes the data to be ploted
reshaped_op_data <- melt(op_run, id=(c("step")), measure.vars=(c("num.opinion.0","num.opinion.1")))

m1 <- mPlot(x = op_run$step, y = c(op_run$num.opinion.0, op_run$num.opinion.1), type = "Line", data = op_run)
m1$set(pointSize = 0, lineWidth = 1)
m1$show('inline', include_assets = TRUE, cdn = TRUE)


