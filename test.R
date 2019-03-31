#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 2019.03.31: Max Lindmark
#
# - Test script for making figures according to their standards with ggplot
#      
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#==== LOAD LIBRARIES AND READ DATA ====
rm(list = ls())

## Load packages
# install.packages("devtools")
# install.packages("ggplot2")
# install.packages("Rcurl")

# *--update package versions if needed!--* 
# packageVersion("RCurl")
library(devtools)     # v2.0.1
library(ggplot2)      # v3.1.0
library(RCurl)        # v1.95.4.10

## Read in data, using Pike as an example

# Q: why mix staple diagrams and lines?
# Unit is in tonnes

dat <- 













