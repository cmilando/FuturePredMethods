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
argvar <- list(fun="ns", knots = quantile(obs$tmean,c(10,75,90)/100, na.rm=T))

maxlag <- 5

arglag <- list(fun="ns", knots=logknots(maxlag, nk=3))

# *******
round_by = 1

Tmean_buffer = 5

x_for_basis <- seq(round(min(obs$tmean) - Tmean_buffer, round_by), 
                   round(max(obs$tmean) + Tmean_buffer, round_by), by = 0.1)

cb <- crossbasis_local(x = x_for_basis, 
                       x_to_match = obs$tmean, round_by = 1,
                       lag =  maxlag, argvar = argvar, arglag = arglag) 

cb_mat <- as.data.frame(as.matrix(cb))

names(cb_mat) <- paste0("cb",names(cb_mat))

obs2 <- cbind(obs, cb_mat)

formula_str = paste('outcome ~ ns(date,df=round(8*length(date)/365.25)) +',
                    paste(names(cb_mat), collapse = " + "))

f1 <- as.formula(formula_str)


m <- glm(f1, data=obs2, family=quasipoisson)
# coef(m)

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
  ylab('Relative Risk') + xlab(expression(Daily~Mean~Temp.~(degree*C))) +
  annotate(geom = 'text', x = -15,y = 2.5, fontface = 'bold',
           label = 'a. Use DLNM to model\nexposure-response relationship', hjust = 0,
           family = ff)
p0

### 
# ok so now, predict I gues

# lets look at the timeseries
ci.level = 0.95
z <- qnorm(1 - (1 - ci.level)/2)
pred_m <- predict(object = m, newdata = obs2, se.fit = T)
exp_est <- exp(pred_m$fit)
exp_ub <- exp(pred_m$fit + z * pred_m$se.fit)
exp_lb <- exp(pred_m$fit - z * pred_m$se.fit)

fit_df <- data.frame(exp_est, exp_ub, exp_lb)
print(head(fit_df[maxlag:(maxlag+100),]))

## right, because of cb, it starts 21 days below, so add 21 0s
# fit_df <- rbind(data.frame(exp_est = rep(NA, maxlag), 
#                            exp_ub= rep(NA, maxlag), 
#                            exp_lb= rep(NA, maxlag)), fit_df)

print(sum(exp_est, na.rm = T))

obs_comb_pres <- cbind(obs2, fit_df)
obs_comb_pres$scen = 'Present E[Y] (95% CI)'
obs_comb_pres$year = year(obs_comb_pres$date)
obs_comb_pres$month = month(obs_comb_pres$date)

col1 = 'lightblue'
col2 = 'blue'

YLIM <- c(150, 230)

library(tidyverse)
library(ggpubr)

p1 <- ggplot(obs_comb_pres %>% filter(year == 1996, month %in% 6:8)) +
  theme_pubr() + #coord_cartesian(ylim = YLIM) +
  scale_fill_manual(values = 'lightblue') +
  scale_color_manual(values = 'blue') +
  geom_line(aes(x = date, y = outcome), color = grey(0.15), linetype = '11') +
  geom_ribbon(aes(x = date, ymin = exp_lb, ymax = exp_ub, fill = scen),  
              alpha = 0.5) +
  geom_line(aes(x = date, y = exp_est, color = scen), show.legend = T) +
  xlab(NULL) + ylab("Daily ED visits per 100k") +
  theme(legend.position.inside = c(0.15, 0.75),
        legend.position = 'inside',
        legend.title = element_blank()) +
  annotate(geom = 'text', x = as.Date('1996-06-01'),
           y = 150, fontface = 'bold',
           label = 'b. Use model to estimate time-series of ED visits in the present', hjust = 0,
           family = ff)

p1

##################################################################################
##################################################################################


set.seed(123)

get_future_df <- function(t_fut) {
  
  cb_fut <- crossbasis_local(x = x_for_basis, 
                             x_to_match = t_fut, round_by = 1,
                             lag =  maxlag, argvar = argvar, arglag = arglag) 
  
  cb_fut_mat <- as.data.frame(as.matrix(cb_fut))
  
  names(cb_fut_mat) <- paste0("cb",names(cb_fut_mat))
  
  obs3 <- cbind(obs, cb_fut_mat)
  
  # lets look at the timeseries
  ci.level = 0.95
  z <- qnorm(1 - (1 - ci.level)/2)
  ## **** VERY IMPORTANTT TO USE NEWDATA
  pred_m2 <- predict(object = m, newdata = obs3, se.fit = T)
  ## *****
  head(pred_m2$fit, 30)
  exp_est <- exp(pred_m2$fit)
  exp_ub <- exp(pred_m2$fit + z * pred_m2$se.fit)
  exp_lb <- exp(pred_m2$fit - z * pred_m2$se.fit)
  
  fit_df <- data.frame(exp_est, exp_ub, exp_lb)
  print(head(fit_df[maxlag:(maxlag+100),]))
  
  ## right, because of cb, it starts 21 days below, so add 21 0s
  # fit_df <- rbind(data.frame(exp_est = rep(NA, maxlag), 
  #                            exp_ub= rep(NA, maxlag), 
  #                            exp_lb= rep(NA, maxlag)), fit_df)
  
  print(sum(exp_est, na.rm = T))
  
  obs_comb <- cbind(obs3, fit_df)
  return(obs_comb)
}

t_fut <- obs$tmean + runif(nrow(obs), 3, 5)
obs_comb1 <- get_future_df(t_fut)
obs_comb1$scen <- '245_A E[Y] (95% CI)'

t_fut <- obs$tmean + runif(nrow(obs), 1, 2)
obs_comb2 <- get_future_df(t_fut)
obs_comb2$scen <- '245_B E[Y] (95% CI)'

t_fut <- obs$tmean + runif(nrow(obs), 2, 4)
obs_comb3 <- get_future_df(t_fut)
obs_comb3$scen <- '245_C E[Y] (95% CI)'

obs_comb4 <- rbind(obs_comb1, obs_comb2, obs_comb3)
head(obs_comb4)
obs_comb4$year = year(obs_comb4$date)
obs_comb4$month = month(obs_comb4$date)

# col1 = 'lightgreen'
# col2 = 'forestgreen'
library(tidyverse)
library(viridisLite)
p2 <- ggplot() +
  theme_pubr() + #coord_cartesian(ylim = YLIM) +
  xlab(NULL) + ylab("Daily ED visits per 100k") +
  #geom_line(aes(x = date, y = all)) +
  ###
  # geom_ribbon(aes(x = date, ymin = exp_lb, ymax = exp_ub), fill = 'lightblue', alpha = 0.75,
  #             data = obs_comb_pres %>% filter(year == 1996, month %in% 6:8)) +
  # geom_line(aes(x = date, y = exp_est), color = 'blue', alpha = 0.75,
  #           data = obs_comb_pres %>% filter(year == 1996, month %in% 6:8)) +
  ###
  geom_ribbon(aes(x = date, ymin = exp_lb, ymax = exp_ub, fill = scen, group = scen), alpha = 0.5,
              data = obs_comb4 %>% filter(year == 1996, month %in% 6:8)) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(3, "Reds")) +
  scale_color_manual(values = RColorBrewer::brewer.pal(3, "Reds")) +
  geom_line(aes(x = date, y = exp_est,  col= scen, group = scen),  #linetype = '22',
            data = obs_comb4 %>% filter(year == 1996, month %in% 6:8)) +
  annotate(geom = 'text', x = as.Date('1996-06-01'),y = 100, fontface = 'bold',
           label = 'b. Predict future visits by GCM', hjust = 0,
           family = ff) +
  theme(legend.position.inside = c(0.15, 0.7),
        legend.position = 'inside',
        legend.title = element_blank())

p2

##################################################################################
##################################################################################



##################################################################################
##################################################################################
# ok Greg's method is to bootstrap this step instead

get_diff_bs <- function(dt, yr, month,
                        present_estimates, present_se,
                        future_estimates, future_se, future_scen) {
  
  n_bootstrap <- 250
  
  differences <- matrix(NA, nrow = n_bootstrap, ncol = length(future_estimates))
  
  for (i in 1:n_bootstrap) {
    
    # Sample from the present distribution for each time point
    present_sample <- rnorm(length(present_estimates), 
                            mean = present_estimates, 
                            sd = present_se)
    
    # Sample from the combined future distribution for each time point
    future_sample <- rnorm(length(future_estimates), 
                           mean = future_estimates, 
                           sd = future_se)
    
    # Calculate the differences
    differences[i, ] <- future_sample - present_sample
  }
  dim(differences)
  
  eCI_lower <- apply(differences, 2, quantile, probs = 0.025, na.rm = T)
  eEst <- apply(differences, 2, quantile, probs = 0.5, na.rm = T)
  eCI_upper <- apply(differences, 2, quantile, probs = 0.975, na.rm = T)
  
  diff_df <- data.frame(date = dt, 
                        year = yr,
                        month = month,
                        scen = future_scen,
                        eCI_lower,
                        eEst,
                        eCI_upper)
  
  differences_df <- data.frame(t(differences))
  diff_df <- cbind(diff_df, differences_df)
  
  return(diff_df)
  
}

present_estimates <- obs_comb_pres$exp_est
present_se <- (obs_comb_pres$exp_ub - obs_comb_pres$exp_lb) / (2 * z)

##
future_estimates <- obs_comb1$exp_est
future_se <- (obs_comb1$exp_ub - obs_comb1$exp_lb) / (2 * z)
future_scen <- '245_A - Present E[Y] (eCI 95%)'

diff1 <- get_diff_bs(obs_comb_pres$date, obs_comb_pres$year, obs_comb_pres$month,
                     present_estimates, present_se, 
                     future_estimates, future_se, future_scen)
##
future_estimates <- obs_comb2$exp_est
future_se <- (obs_comb2$exp_ub - obs_comb2$exp_lb) / (2 * z)
future_scen <- '245_B - Present E[Y] (eCI 95%)'

diff2 <- get_diff_bs(obs_comb_pres$date, obs_comb_pres$year, obs_comb_pres$month,
                     present_estimates, present_se, 
                     future_estimates, future_se, future_scen)
##
future_estimates <- obs_comb3$exp_est
future_se <- (obs_comb3$exp_ub - obs_comb3$exp_lb) / (2 * z)
future_scen <- '245_C - Present E[Y] (eCI 95%)'

diff3 <- get_diff_bs(obs_comb_pres$date, obs_comb_pres$year, obs_comb_pres$month,
                     present_estimates, present_se, 
                     future_estimates, future_se, future_scen)

comb_diff <- rbind(diff1, diff2, diff3)

p3b <- ggplot() +
  theme_pubr() + #coord_cartesian(ylim = YLIM) +
  xlab(NULL) + ylab("Change in Daily ED visits per 100k") +
  #geom_line(aes(x = date, y = all)) +
  ###
  # geom_ribbon(aes(x = date, ymin = exp_lb, ymax = exp_ub), fill = 'lightblue', alpha = 0.75,
  #             data = obs_comb_pres %>% filter(year == 1996, month %in% 6:8)) +
  # geom_line(aes(x = date, y = exp_est), color = 'blue', alpha = 0.75,
  #           data = obs_comb_pres %>% filter(year == 1996, month %in% 6:8)) +
  ###
  geom_ribbon(aes(x = date, ymin = eCI_lower, ymax = eCI_upper, fill = scen, group = scen), alpha = 0.5,
              data = comb_diff %>% filter(year == 1996, month %in% 6:8)) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(3, "Greens")) +
  scale_color_manual(values = RColorBrewer::brewer.pal(3, "Greens")) +
  geom_line(aes(x = date, y = eEst,  col= scen, group = scen),  #linetype = '22',
            data = comb_diff %>% filter(year == 1996, month %in% 6:8)) +
  annotate(geom = 'text', x = as.Date('1996-06-01'),y = 50, fontface = 'bold',family = ff,
           label = 'c. Bootstrap differences by model,\nusing DLNM Est and SE', hjust = 0) +
  geom_hline(yintercept = 0, linetype = '41') +
  theme(legend.position.inside = c(0.15, 0.7),
        legend.position = 'inside',
        legend.title = element_blank())

p3b

##################################################################################
##################################################################################



##################################################################################
##################################################################################
# ok now bootrap
library(pbapply)
library(future)
library(future.apply)
plan(multisession)
n_bootstrap <- 250

# make a comb_mat to sample from
# this should be length(dt) x 3000
dim(comb_diff)
cdiff2 <- cbind(diff1, diff2[, 8:(250-1)], diff3[, 8:(250-1)])
cdiff2_mat <- as.matrix(cdiff2[, 8:ncol(cdiff2)])
dim(cdiff2_mat)

# Initialize matrix to store the differences
differences <- matrix(NA, nrow = n_bootstrap, ncol = length(present_estimates))

# Sample from the combined future distribution for each time point
sample1 <- function(x) sample(x, 1)

# Generate bootstrap samples and calculate differences
differences <- pbsapply(1:n_bootstrap, function(i) apply(cdiff2_mat, 1, sample1),
                        cl = 'future')
dim(differences)

# Calculate empirical confidence intervals
eCI_lower <- apply(differences, 1, quantile, probs = 0.025, na.rm = T)
eEst <- apply(differences, 1, quantile, probs = 0.5, na.rm = T)
eCI_upper <- apply(differences, 1, quantile, probs = 0.975, na.rm = T)

diff_df <- data.frame(date = obs_comb_pres$date, 
                      year = obs_comb_pres$year,
                      month = obs_comb_pres$month,
                      eCI_lower,
                      eEst,
                      eCI_upper)

diff_df$scen = '245_* (95% eCI)'

p4b <- ggplot(diff_df %>% filter(year == 1996, month %in% 6:8)) +
  theme_pubr() + 
  geom_ribbon(aes(x = date, ymin = eCI_lower, ymax = eCI_upper, fill = scen), 
              alpha = 0.5) +
  scale_fill_manual(values = c('lightgreen')) +
  scale_color_manual(values = c('forestgreen')) +
  geom_line(aes(x = date, y = eEst, color = scen), show.legend = T) +
  # geom_ribbon(aes(x = date, ymin = exp_lb, ymax = exp_ub, fill = scen),  alpha = 0.75,
  #             data = obs_comb4_comb %>% filter(year == 1996, month %in% 6:8)) +
  # geom_line(aes(x = date, y = exp_est, color = scen), alpha = 0.75,
  #           data = obs_comb4_comb %>% filter(year == 1996, month %in% 6:8)) +
  xlab(NULL) + ylab("Change in Daily ED visits per 100k") +
  geom_hline(yintercept = 0, linetype = '41') +
  annotate(geom = 'text', x = as.Date('1996-06-01'),y = 45, fontface = 'bold',
           family = ff,
           label = 'd. Bootstrap the combined\ndifference', hjust = 0) +
  theme(legend.position.inside = c(0.15, 0.7),
        legend.position = 'inside',
        legend.title = element_blank())

p4b

##################################################################################
##################################################################################

library(patchwork)
library(extrafont)
loadfonts()
extrafont::loadfonts()
extrafont::fonttable()


#pdf("test_methods.pdf", width = 11/3*2, height = 6)
p1 +   theme(legend.position.inside = c(0.32, 0.75), 
             legend.key = element_blank(), 
             legend.text = element_text(family = ff),
             axis.text = element_text(family = ff),
             axis.title = element_text(family = ff),
             legend.background = element_blank()) +
  p2 + theme(legend.position.inside = c(0.30, 0.71), 
             legend.key = element_blank(), 
             legend.text = element_text(family = ff),
             axis.text = element_text(family = ff),
             axis.title = element_text(family = ff),
             legend.background = element_blank()) +
  p3b + theme(legend.position.inside = c(0.4, 0.68), 
              legend.key = element_blank(), 
              legend.text = element_text(family = ff),
              axis.text = element_text(family = ff),
              axis.title = element_text(family = ff),
              legend.background = element_blank()) +
  p4b + theme(legend.position.inside = c(0.3, 0.75), 
              legend.key = element_blank(), 
              legend.text = element_text(family = ff),
              axis.text = element_text(family = ff),
              axis.title = element_text(family = ff),
              legend.background = element_blank()) +
  plot_layout(ncol = 2, nrow = 2)
#dev.off()

ggsave("test_methods.png", width = 8, height = 6.5, dpi = 600)
