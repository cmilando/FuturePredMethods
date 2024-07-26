library(dlnm) ; 
library(splines) ; 
source("crossbasis_local.R")
library(future); 
library(future.apply)
library(tidyverse)
library(ggpubr)

ff <- "Helvetica"
##################################################################################
##################################################################################
source('00_create_data.R')

##################################################################################
##################################################################################


# graph 1
# reduced complexity, normally would do more knots
argvar <- list(fun="ns", knots = quantile(obs$tmean,c(50)/100, na.rm=T))

maxlag <- 2

# reduced complexity
arglag <- list(fun="ns", knots=logknots(maxlag, nk=1))

# *******
round_by = 1

Tmean_buffer = 5

x_for_basis <- seq(round(min(obs$tmean) - Tmean_buffer, round_by), 
                   round(max(obs$tmean) + Tmean_buffer, round_by), by = 0.1)

cb <- crossbasis_local(x = x_for_basis, 
                       x_to_match = obs$tmean, round_by = round_by,
                       lag =  maxlag, argvar = argvar, arglag = arglag) 
head(cb)

cb_mat <- as.data.frame(as.matrix(cb))

names(cb_mat) <- paste0("cb",names(cb_mat))

obs2 <- cbind(obs, cb_mat)
head(obs2)

formula_str = paste('outcome ~ ns(date,df=round(8*length(date)/365.25)) +',
                    paste(names(cb_mat), collapse = " + "))

f1 <- as.formula(formula_str)
f1

m <- glm(f1, data=obs2, family=quasipoisson)
coef(m)

# identical(round(coef(m_true), 4), round(coef(m), 4))
cp <- crosspred(cb, m, cen = 20)
cp_df <- data.frame(
  x = cp$predvar,
  est = cp$allRRfit,
  lb = cp$allRRlow,
  ub = cp$allRRhigh
)

p0 <- ggplot(cp_df) +
  theme_pubr() + #coord_cartesian(ylim = YLIM) +
  geom_hline(yintercept = 1, linetype = '11') +
  geom_ribbon(aes(x = x, ymin = lb, ymax = ub),  
              alpha = 0.5, fill = grey(0.75)) + 
  geom_line(aes(x = x, y = est), color = 'blue') +
  ylab('Relative Risk') + #xlab(expression(Daily~Mean~Temp.~(degree*C))) +
  coord_cartesian(clip = 'off') +
  scale_x_continuous(breaks = c(0, 20, 40),
                     labels = c(expression(0~degree*C), 
                                expression(20~degree*C), 
                                expression(40~degree*C))) +
  xlab(NULL) +
  #theme(axis.ticks.x = element_blank()) +
  annotate(geom = 'text', x = -15, y = 1.315, fontface = 'bold',
           label = 'a. Use DLNM to model temperature-ED\nvisits relationship', 
           hjust = 0,
           family = ff)
p0
ggsave("p0.png", width = 10/3, height = 5/2, dpi = 600)


