# -----------------------------------------------------------------------------
# 
# Plot 2
#
# -----------------------------------------------------------------------------
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
  exp_est <- exp(pred_m2$fit)
  exp_ub <- exp(pred_m2$fit + z * pred_m2$se.fit)
  exp_lb <- exp(pred_m2$fit - z * pred_m2$se.fit)
  
  fit_df <- data.frame(exp_est, exp_ub, exp_lb)
  
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

p2 <- ggplot() +
  theme_pubr() + 
  xlab(NULL) + ylab("Daily ED visits per 100k") +
  geom_ribbon(aes(x = date, ymin = exp_lb, ymax = exp_ub, fill = scen, group = scen), alpha = 0.5,
              data = obs_comb4 %>% filter(year == 1996, month %in% 6:8)) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(3, "Reds")) +
  scale_color_manual(values = RColorBrewer::brewer.pal(3, "Reds")) +
  geom_line(aes(x = date, y = exp_est,  col= scen, group = scen),  
            data = obs_comb4 %>% filter(year == 1996, month %in% 6:8)) +
  annotate(geom = 'text', x = as.Date('1996-06-01'),
           y = 125, fontface = 'bold',
           label = 'c. Predict future visits by GCM', hjust = 0,
           family = ff) +
  theme(legend.position.inside = c(0.15, 0.7),
        legend.position = 'inside',
        legend.title = element_blank())


