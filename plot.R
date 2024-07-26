source("00.R")
source("01.R")
source("02.R")
source("03.R")
source("04.R")
source("05.R")

library(patchwork)
library(extrafont)
loadfonts()
extrafont::loadfonts()
extrafont::fonttable()
lsize = 9

#pdf("test_methods.pdf", width = 11/3*2, height = 6)
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
#dev.off()

ggsave("methods_fig1.png", width = 12, height = 6, dpi = 600)
