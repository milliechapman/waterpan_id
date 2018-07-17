## elephant water hole analysis
# Khaudum NP 
# This script generates figures for the elephant movement, recursions, and water pan ID work
# Millie Chapman & Will Oestreich 16 July 2018

library(tidyr)
rm(list = ls())

#####
# Figure 1
# Map of Namibia + zoom-in on Khaudum NP. Includes locations at which elephants in the analysis were tagged.


#####
# Figure 2
# (a) Dry season elephant mvmts; (b) dry season high recursion points; (c) wet season elephant mvmts; 
# (d) wet season high recursion points.

# load data
ele_dry_mvmt <- read.csv("outputs/ele_dry_mvmt.csv")
ele_wet_mvmt <- read.csv("outputs/ele_wet_mvmt.csv")
ele_dryRt <- read.csv("outputs/ele_dryRt.csv")
ele_wetRt <- read.csv("outputs/ele_wetRt.csv")

# plots
par(mfrow=c(2,2))
# (a)
plot(ele_dry_mvmt$X,ele_dry_mvmt$Y)
# (b)
plot(ele_dryRt$X,ele_dryRt$Y, col = "red", xlim=c(1040000, 1160000), ylim=c(-2200000, -2000000), 
     main = "Dry season high recursion points (200m radius)")
# (c)
plot(ele_wet_mvmt$X,ele_wet_mvmt$Y)
# (d)
plot(ele_wetRt$X,ele_wetRt$Y, col = "red", xlim=c(1040000, 1160000), ylim=c(-2200000, -2000000), 
     main = "Wet season high recursion points (200m radius)")

#####
# Figure 3
# Difference in high recursion points between wet and dry seasons. 
# By removing wet season high recursion points that also show up
# during dry season, we should be left with only precipitation-driven 
# high recursion points for the dry season.


#####
# Figure 4
# Maps showing potential pan centroids w/ circles for wet season 
# recursions at these points overlaid. Reveals likely natural pan 
# locations for (a) 2013, (b) 2014, and (c) 2015.






