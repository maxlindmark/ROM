#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 2019.03.31: Max Lindmark
#
# - Test script for making figures according to their standards with ggplot
#
# A. READ LIBRARIES AND DATA
#
# B. TEST THE MOST COMPLEX FIGURE
#
# C. USE THE SAME CODE TO PRODUCE THE SIMPLEST FIGURE
#      
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#==== A. LOAD LIBRARIES AND READ DATA ====
rm(list = ls())

## Load packages
# install.packages("devtools")
# install.packages("ggplot2")
# install.packages("Rcurl")
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("scales")

# .libPaths()
# .libPaths("C:/Program Files/R/R-3.5.0/library")

# packageVersion("scales")
library(devtools) # v2.0.1
library(ggplot2)  # v3.1.0
library(RCurl)    # v1.95.4.10
library(tidyr)    # v0.8.1    
library(dplyr)    # v0.7.5
library(scales)   # v0.5.0

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

# This palette is used to make the line white for the rec fisheries. I use the other to get the basic palette in the legend though (most likely). This palette must give "white" in the right order, i.e. the order so that it matches rec fish in the data!
pal_line <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#E69F00", "white")

## Now we need to tell ggplot to plot to use the order of the data and not the alphabetical order

unique(dat$Sjö)

dat$Sjö <- as.factor(dat$Sjö)

dat$Sjö <- factor(dat$Sjö, # rename here, don't call it Sjö
                  levels = c("Stora sjöarna", 
                             "Vänern", 
                             "Vättern", 
                             "Mälaren", 
                             "Hjälmaren", 
                             "Fritidsfiske"))

#==== B. MOST COMPLEX PLOT ====

p <- ggplot(dat, aes(År, Ton, color = Sjö)) +
  geom_bar(data = subset(dat, Sjö == "Stora sjöarna"), 
            aes(x = År, y = Ton), stat = "identity", color = pal[1], fill = pal[1], 
            width = 0.6) +
  geom_line(size = 1) +
  geom_point(data = subset(dat, Sjö == "Fritidsfiske"), 
             aes(År, Ton, fill = Sjö), size = 2, color = pal[6]) +
  geom_errorbar(data = subset(dat, Sjö == "Fritidsfiske"), 
                aes(x = År, ymin = rec_minu, ymax = rec_plus), 
                show.legend = FALSE, width  = 1, color = pal[6]) +
  scale_color_manual(values = pal_line) +
  labs(x = "", y = "Landningar (ton)") +
  ggtitle("Landningar") +
  guides(fill = FALSE,
         color = guide_legend(nrow = 3, 
                              title = "",
                              override.aes = list(size = 1.3, 
                                                  color = pal_line), # set pal if you want color in legend for Fritidsfiske, but then it's a line!
                              keywidth = 0.3,
                              keyheight = 0.1,
                              default.unit = "inch")) +
  scale_x_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 6)) +
  scale_y_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 5)) +
  theme_bw(base_size = 12) +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        axis.ticks.length = unit(0.05, "cm"),
        axis.line = element_line(colour = "black", size = 0.3), 
        text = element_text(family = "sans"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5, margin = margin(b = -3),
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

ggsave("fig_full.tiff", plot = p, dpi = 300, width = 8, height = 8, units = "cm")


#==== C. SIMPLE PLOT FROM FULL (COMPLEX) CODE ====
# code from old complex plot, need to update to make sure it's still playing with the 
# modified version of the complex plot (i.e. the palette hacks)

p2 <- ggplot(dat, aes(År, Ton)) +
  geom_bar(data = subset(dat, Sjö == "Stora sjöarna"), 
           aes(x = År, y = Ton, fill = Sjö), stat = "identity", 
           width = 0.6) +
  #geom_line(size = 1) +
  #geom_errorbar(data = dat, aes(x = År, ymin = rec_minu, ymax = rec_plus), 
  #              show.legend = FALSE, width  = 1) +
  scale_fill_manual(values = pal) +
  
  labs(x = "", y = "Landningar (ton)") +
  ggtitle("Landningar") +
  guides(fill = guide_legend(nrow = 3, 
                             title = "",
                             override.aes = list(size = 1.3),
                             keywidth = 0.3,
                             keyheight = 0.1,
                             default.unit = "inch")) +
  scale_x_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 6)) +
  scale_y_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 5)) +
  theme_bw(base_size = 12) +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        axis.ticks.length = unit(0.05, "cm"),
        axis.line = element_line(colour = "black", size = 0.3), 
        text = element_text(family = "sans"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5, margin = margin(b = -3),
                                  size = 9.6, face = "bold"),
        legend.position = c(0.5, -0.25), 
        legend.text = element_text(size = 8),
        legend.justification = "bottom", 
        legend.background = element_rect(fill = "transparent"), 
        legend.key = element_rect(fill = "transparent"),
        aspect.ratio = 1,
        plot.margin = unit(c(5.5, 5.5, 20, 5.5), "points")) +
  NULL  

p2

ggsave("fig_small.tiff", plot = p2, dpi = 300, width = 8, height = 8, units = "cm")


#======== C. TO DO ========
# axis tick labels (year and tonnes ? Seems buggy, what is it that they want, a fixed number of ticks, from min to max?  Wait until they reply about that
# axis width (currently 0.2) 
