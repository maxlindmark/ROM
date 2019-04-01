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
# packageVersion("tidyr")
library(devtools) # v2.0.1
library(ggplot2)  # v3.1.0
library(RCurl)    # v1.95.4.10
library(tidyr)    # v0.8.1    
library(dplyr)    # v0.7.5

## Read in data, using Pike as an example

# Q: why mix staple diagrams and lines?
# Unit is in tonnes

dat <- read.csv("gädda.csv", sep = ";", dec = ",")

dat <- dat %>% rename(Fritidsfiske = Fritidsfiske..stora.sjöarna..inklusive.Storsjön.,
                      `Stora sjöarna` = Stora.sjöarna)

head(dat)
dat$rec_plus <- dat$Fritidsfiske + dat$error
dat$rec_minu <- dat$Fritidsfiske - dat$error

## Make data long 
dat <- dat %>% gather(Sjö, Ton, 2:7)

head(dat)

unique(dat$Sjö)

pal <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#E69F00", "#D55E00")


p <- ggplot(dat, aes(År, Ton, color = Sjö)) +
   geom_bar(data = subset(dat, Sjö == "Stora sjöarna"), 
            aes(x = År, y = Ton), stat = "identity", color = pal[1], fill = pal[1], 
            width = 0.6) +
  geom_line(size = 1) +
  geom_errorbar(data = dat, aes(x = År, ymin = rec_minu, ymax = rec_plus), 
                show.legend = FALSE, width  = 1) +
  scale_color_manual(values = pal) +
  
  labs(x = "", y = "Landningar (ton)") +
  ggtitle("Landningar") +
  theme_bw(base_size = 12) +
  guides(color = guide_legend(nrow = 3, 
                              title = "",
                              override.aes = list(size = 1.3),
                              keywidth = 0.3,
                              keyheight = 0.1,
                              default.unit = "inch")) +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +

  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        axis.ticks.length = unit(0.05, "cm"),
        axis.line = element_line(colour = "black", size = 0.3), 
        text = element_text(family = "sans"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5, margin = margin(b = -13),
                                  size = 9.6, face = "bold"),
        legend.position = c(0.5, -0.25), 
        legend.text = element_text(size = 8),
        legend.justification = "bottom", 
        legend.background = element_rect(fill = "transparent"), 
        legend.key = element_rect(fill = "transparent"),
        aspect.ratio = 1,
        plot.margin = unit(c(5.5, 5.5, 20, 5.5), "points")) +
  NULL  

p

ggsave("fig_test.tiff", plot = p, dpi = 300, width = 8, height = 8, units = "cm")


#### TO DO ####
# axis tick labels (year and tonnes ?
# axis width (currently 0.2) 
# let color follow order of data:
# Read this on reorder and coloring: https://stackoverflow.com/questions/38131596/ggplot2-geom-bar-how-to-keep-order-of-data-frame
# and this: https://stackoverflow.com/questions/3253641/change-the-order-of-a-discrete-x-scale
# make the raw data long and clean, do not want the user to do that

# See what the plot looks like when I don't have that many levels. Do margins look ok?




