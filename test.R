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
# packageVersion("dplyr")
library(devtools) # v2.0.1
library(ggplot2)  # v3.1.0
library(RCurl)    # v1.95.4.10
library(tidyr)    # v0.7.5    

## Read in data, using Pike as an example

# Q: why mix staple diagrams and lines?
# Unit is in tonnes

dat <- read.csv("gädda.csv", sep = ";", dec = ",")

## Make data long
dat <- dat %>% gather(Sjö, Ton, 2:6)

head(dat)

ggplot(dat, aes(År, Ton, color = Sjö)) +
  geom_line() +
  geom_point(data = dat, aes(År, Fritidsfiske..stora.sjöarna..inklusive.Storsjön.)) + 
NULL  

# Need to fix legend for points inside the line-legend... https://stackoverflow.com/questions/26587940/ggplot2-different-legend-symbols-for-points-and-lines

# Let's first to the other layouts...

# Create color pallette



# read this: https://ben-williams.github.io/updated_ggplot_figures.html#fonts-and-resolution

# and this: https://stackoverflow.com/questions/43345752/export-graphics-with-exact-size-and-font-size

pal <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#E69F00", "D55E00")

p <- ggplot(dat, aes(År, Ton, color = Sjö)) +
  geom_line() +
  geom_point(data = dat, aes(År, Fritidsfiske..stora.sjöarna..inklusive.Storsjön.)) + 
  scale_color_manual(values = pal) +
  labs(x = "", y = "Landningar (ton)") +
  ggtitle("Landningar") +
  theme_bw(base_size = 12) +
  guides(color = guide_legend(nrow = 2, 
                              byrow = TRUE, 
                              title = "")) +
  guides(color = F) +
  
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        text = element_text(family = "sans"),
        panel.background = element_rect(fill = "white", colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, margin = margin(t = 10, b = -20),
                                  size = 9.6, face = "bold"),
        legend.position = "bottom", 
        legend.text = element_text(size = 8, margin = margin(l = -5), hjust = 0), 
        element_line(size = 0.2),
        aspect.ratio = 1) +
  NULL  

p

ggsave("fig_test2.tiff", plot = p, dpi = 300, width = 8, height = 8, units = "cm")

# Looks sort of OK with font sizes.. Need to check: 

# axis width (currently 0.2) 
# legend key spacing (much tighter)
# I need to make all as points likely, and then add lines with a subset? or read the link above..
# ggtitle margin
# add errorbars
# can probably increase line and point size slightly
# number of ticks for years and landings (start and end point e.g.)
# top and right axis color white
# 


# when I'm finsihed I'll read this: https://stackoverflow.com/questions/55426796/set-defaults-traits-for-guide-colorbar

