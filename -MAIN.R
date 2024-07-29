# -----------------------------------------------------------------------------
# 
# Run all
#
# -----------------------------------------------------------------------------
library(dlnm) 
library(splines) 
library(future)
library(future.apply)
library(tidyverse)
library(ggpubr)
library(viridisLite)
library(lemon)
library(extrafont)
library(patchwork)
library(extrafont)

# -----------------------------------------------------------------------------
plan(multisession)
options(future.globals.maxSize = 1200*1024^2)
extrafont::loadfonts()
extrafont::fonttable()
loadfonts()
ff <- "Helvetica"

## modified version of dlnm::crossbasis that extends basis
source("crossbasis_local.R") 

# -----------------------------------------------------------------------------
source("00_create_data.R")
source("00.R")
source("01.R")
source("02.R")
source("03.R")
source("04.R")
source("05.R")
# -----------------------------------------------------------------------------

lsize = 9

p0 +  theme(axis.title.x = element_blank(),
            legend.text = element_text(family = ff, size = lsize)) +
  #
  p1 +   theme(legend.position.inside = c(0.31, 0.86), 
             legend.key = element_blank(), 
             legend.text = element_text(family = ff, size = lsize),
             axis.text = element_text(family = ff),
             axis.title = element_text(family = ff),
             legend.background = element_blank()) +
  #
  p2 + theme(legend.position.inside = c(0.31, 0.77), 
             legend.key = element_blank(), 
             legend.text = element_text(family = ff, size = lsize),
             axis.text = element_text(family = ff),
             axis.title = element_text(family = ff),
             legend.background = element_blank()) +
  #
  p3b + theme(legend.position.inside = c(0.4, 0.73), 
              legend.key = element_blank(), 
              legend.text = element_text(family = ff, size = lsize),
              axis.text = element_text(family = ff),
              axis.title = element_text(family = ff),
              legend.background = element_blank()) +
  #
  p4b + theme(legend.position.inside = c(0.27, 0.86), 
              legend.key = element_blank(), 
              legend.text = element_text(family = ff, size = lsize),
              axis.text = element_text(family = ff),
              axis.title = element_text(family = ff),
              legend.background = element_blank()) +
  #
  p5 + theme(legend.position.inside = c(0.27, 0.81), 
          legend.key = element_blank(), 
          legend.text = element_text(family = ff, size = lsize),
          axis.text = element_text(family = ff, size = 12, color = 'black'),
          axis.title = element_text(family = ff),
          legend.background = element_blank()) + 
  plot_layout(ncol = 3, nrow = 2) 


ggsave("methods_fig1.png", width = 12, height = 6, dpi = 600)
